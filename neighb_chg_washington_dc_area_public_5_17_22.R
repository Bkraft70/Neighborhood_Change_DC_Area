# Neighborhood Change in the Washington DC Area

# install tidyverse and tidycensus if not already installed
# install.packages("tidyverse")
# install.packages("tidycensus")

# install "tmap" and "sf" packages if not already installed - for creating shapefile later in the script
# install.packages("sf")
# install.packages("tmap")

library(tidyverse)
library(tidycensus)

####################

# load census variables if curious
v19 <- load_variables(2019, "acs5", cache = TRUE)
v00 <- load_variables(2000, "sf3", cache = TRUE)

####################

# Import two files
# 1) NBER core based statistical area (cbsa) to fips crosswalk (downloaded from https://www.nber.org/research/data/census-core-based-statistical-area-cbsa-federal-information-processing-series-fips-county-crosswalk)
# 2) Brown University Longitudinal Tract Database (LTDB) (downloaded from https://s4.ad.brown.edu/Projects/Diversity/Researcher/Bridging.htm)

# cbsa to fips crosswalk
cbsa2fipsxw <- read_csv("https://raw.githubusercontent.com/Bkraft70/Neighborhood_Change_DC_Area/main/cbsa2fipsxw.csv",
                        col_types = cols(cbsacode = col_character(),
                                         csacode = col_character(), fipscountycode = col_character(),
                                         fipsstatecode = col_character())) %>%
  mutate(state_pad = str_pad(fipsstatecode, 2, "left", pad = "0"),
         county_pad = str_pad(fipscountycode, 3, "left", pad = "0")) %>%
  mutate(countyfips = paste(state_pad, county_pad, sep = ""))


# ltdb
ltdb_crosswalk_2000_2010 <- read_csv("https://raw.githubusercontent.com/Bkraft70/Neighborhood_Change_MoCo/main/crosswalk_2000_2010.csv",
                                     col_types = cols(cbsa10 = col_character(),
                                                      metdiv10 = col_character(),
                                                      placefp10 = col_character()))

####################

# create list of DMV counties
# DMV = DC metropolitan statistical area
# can change or add msas
dmv_counties <- cbsa2fipsxw %>%
  filter(cbsacode == 47900)


####################

# Extract 2019 ACS variables
variables19 <- map2_dfr(
  dmv_counties$state_pad, dmv_counties$county_pad,
  # loops over state fips code and county fips code in dmv_counties dataframe
  # so only have to run one time for all jurisdictions
  ~ get_acs(
    geography = "tract",
    variables = c(n_totpop19 = "B01003_001",
                  n_agginc19 = "B19313_001",
                  n_totpoppov19 = "C17002_001",
                  n_ov200pctpov19 = "C17002_008",
                  n_50to99pctpov19 = "C17002_003",
                  #2019 ACS has 50% - 99% all in one variable;
                  #in 2000, it is disaggregated into 50-74% and 75-99%
                  n_u50pctpov19 = "C17002_002",
                  n_totpopraceth19 = "B03002_001",
                  n_nlwt19 = "B03002_003",
                  n_nlblk19 = "B03002_004",
                  n_nlamin19 = "B03002_005",
                  n_nlasi19 = "B03002_006",
                  n_nlhp19 = "B03002_007",
                  n_nlso19 = "B03002_008",
                  n_nl2om19 = "B03002_009",
                  # _010 and _011 are included in _009
                  n_lat19 = "B03002_012",
                  n_hu19 = "B25001_001",
                  n_huten19 = "B25003_001",
                  n_huown19 = "B25003_002",
                  n_hurent19 = "B25003_003"),
    state = .x,
    county = .y,
    survey = "acs5",
    year = 2019)) %>%
  pivot_wider(id_cols = GEOID, names_from = variable, values_from = estimate) %>%
  mutate(n_upov19 = n_50to99pctpov19 + n_u50pctpov19,
         # collapse under 100% of povery variables into one
         n_u200pctpov19 = n_totpoppov19-n_ov200pctpov19) %>%
  # create under 200% of poverty
  mutate(n_blklat19 = n_nlblk19 + n_lat19) %>%
  # create a black & latino combined category in case needed for future analysis
  select(GEOID:n_u50pctpov19, n_upov19, n_u200pctpov19, n_totpopraceth19:n_lat19, n_blklat19, everything())

# Extract 2000 Decennial Census Variables
variables00 <- map2_dfr(
  dmv_counties$state_pad, dmv_counties$county_pad,
  ~ get_decennial(geography = "tract",
                  # leave off "n_" prefix in 2000 data for now - will be added in next step
                  variables = c(totpop00 = "P001001",
                                agginc00 = "P083001",
                                totpoppov00 = "P088001",
                                ov200pctpov00 = "P088010",
                                x75to99pctpov00 = "P088004",
                                x50to74pctpov00 = "P088003",
                                #2000 SF3 splits 50 to 99 pct into two variables;
                                #they're collapsed in 2019 ACS
                                u50pctpov00 = "P088002",
                                totpopraceth00 = "P007001",
                                nlwt00 = "P007003",
                                nlblk00 = "P007004",
                                nlamin00 = "P007005",
                                nlasi00 = "P007006",
                                nlhp00 = "P007007",
                                nlso00 = "P007008",
                                nl2om00 = "P007009",
                                lat00 = "P007010",
                                hu00 = "H001001",
                                huten00 = "H007001",
                                huown00 = "H007002",
                                hurent00 = "H007003"),
                  state = .x,
                  county = .y,
                  sumfile = "sf3",
                  year = 2000,
                  show_call = TRUE)) %>%
  pivot_wider(id_cols = GEOID, names_from = variable, values_from = value) %>%
  mutate(x50to99pctpov00 = x75to99pctpov00 + x50to74pctpov00) %>%
  # collapse these categories to match 2019 
  select(-x75to99pctpov00, -x50to74pctpov00) %>%
  # remove original categories
  mutate(upov00 = x50to99pctpov00 + u50pctpov00,
         u200pctpov00 = totpoppov00-ov200pctpov00) %>%
  mutate(blklat00 = nlblk00 + lat00) %>%
  # create a black & latino combined category in case needed for future analysis
  select(GEOID:ov200pctpov00, x50to99pctpov00, upov00, u200pctpov00, totpopraceth00:lat00, blklat00, everything()) %>%
  # change name of GEOID field to trtid00 to match ltdb id
  # ultimately will name final tract identifier "geoid10"
  dplyr::rename(trtid00 = GEOID)

####################
# join 2000 variables to ltdb crosswalk
variables00_with_ltdb <- variables00 %>%
  inner_join(ltdb_crosswalk_2000_2010, by = "trtid00")
# this step will add additional records because tracts that were split have multiple records

####################
# apply ltdb weights to 2000 values
# name variables with "n" prefix for county and "00" suffix for year
variables00_xwalked <- variables00_with_ltdb %>%
  mutate(n_totpop00 = totpop00*weight,
         n_agginc00 = agginc00*weight,
         n_totpoppov00 = totpoppov00*weight,
         n_ov200pctpov00 = ov200pctpov00*weight,
         n_50to99pctpov00 = x50to99pctpov00*weight,
         n_u50pctpov00 = u50pctpov00*weight,
         n_upov00 = upov00*weight,
         n_u200pctpov00 = u200pctpov00*weight,
         n_totpopraceth00 = totpopraceth00*weight,
         n_nlwt00 = nlwt00*weight,
         n_nlblk00 = nlblk00*weight,
         n_nlamin00 = nlamin00*weight,
         n_nlasi00 = nlasi00*weight,
         n_nlhp00 = nlhp00*weight,
         n_nlso00 = nlso00*weight,
         n_nl2om00 = nl2om00*weight,
         n_lat00 = lat00*weight,
         n_blklat00 = blklat00*weight,
         n_hu00 = hu00*weight,
         n_huten00 = huten00*weight,
         n_huown00 = huown00*weight,
         n_hurent00 = hurent00*weight) %>%
  # sum by 2010 tract id
  group_by(trtid10) %>%
  summarize(n_totpop00 = sum(n_totpop00),
            n_agginc00 = sum(n_agginc00),
            n_totpoppov00 = sum(n_totpoppov00),
            n_ov200pctpov00 = sum(n_ov200pctpov00),
            n_50to99pctpov00 = sum(n_50to99pctpov00),
            n_u50pctpov00 = sum(n_u50pctpov00),
            n_upov00 = sum(n_upov00),
            n_u200pctpov00 = sum(n_u200pctpov00),
            n_totpopraceth00 = sum(n_totpopraceth00),
            n_nlwt00 = sum(n_nlwt00),
            n_nlblk00 = sum(n_nlblk00),
            n_nlamin00 = sum(n_nlamin00),
            n_nlasi00 = sum(n_nlasi00),
            n_nlhp00 = sum(n_nlhp00),
            n_nlso00 = sum(n_nlso00),
            n_nl2om00 = sum(n_nl2om00),
            n_lat00 = sum(n_lat00),
            n_blklat00 = sum(n_blklat00),
            n_hu00 = sum(n_hu00),
            n_huten00 = sum(n_huten00),
            n_huown00 = sum(n_huown00),
            n_hurent00 = sum(n_hurent00)) %>%
  ungroup


####################
# join 2000 crosswalked variables to 2019 variables
neighb_chg_base_pre_cleanup <- variables19 %>%
  full_join(variables00_xwalked, by = c("GEOID" = "trtid10")) %>%
  dplyr::rename(geoid10 = GEOID)

####################
# look for and remove inconsistent or anomalous tracts 
odd_tracts <- neighb_chg_base_pre_cleanup %>%
  filter(is.na(n_totpop19) | is.na(n_totpop00) | n_totpop19 < 1000 | n_totpop00 < 10)
# filters for tracts that have "NAs" in population fields due to lack of continuity from
# 2000 to 2019, fewer than 1000 in population in 2019 (tracts are supposed to have at least 1200),
# or fewer than 10 in 2000, which would indicate a potential misallocation from the crosswalk
# See "Tract Cleanup Documentation" for rationale for including or excluding specific tracts that raised questions.
# "tracts to remove" spreadsheet was generate manually from investigating "odd_tracts" and other anomalies

# Import "tracts to remove" spreadsheet
nc_tracts_to_remove <- read_csv("https://raw.githubusercontent.com/Bkraft70/Neighborhood_Change_DC_Area/main/nc_tracts_to_remove.csv",
                                col_types = cols(col_character()))

# remove tracts that don't belong in analysis
neighb_chg_base <- neighb_chg_base_pre_cleanup %>%
  filter(!geoid10 %in% nc_tracts_to_remove$geoid10)

####################
# create derived variables
neighb_chg_base_derived_vars <- neighb_chg_base %>%
  mutate(
    # poverty variable shares of tracts
    s_u50pctpov00 = n_u50pctpov00/n_totpoppov00,
    s_u50pctpov19 = n_u50pctpov19/n_totpoppov19,
    s_50to99pctpov00 = n_50to99pctpov00/n_totpoppov00,
    s_50to99pctpov19 = n_50to99pctpov19/n_totpoppov19,
    s_upov00 = n_upov00/n_totpoppov00,
    s_upov19 = n_upov19/n_totpoppov19,
    s_u200pctpov00 = n_u200pctpov00/n_totpoppov00,
    s_u200pctpov19 = n_u200pctpov19/n_totpoppov19,
    s_ov200pctpov00 = n_ov200pctpov00/n_totpoppov00,
    s_ov200pctpov19 = n_ov200pctpov19/n_totpoppov19,
    # absolute changes
    a_u50pctpov = n_u50pctpov19 - n_u50pctpov00,
    a_50to99pctpov = n_50to99pctpov19 - n_50to99pctpov00,
    a_upov = n_upov19 - n_upov00,
    a_u200pctpov = n_u200pctpov19 - n_u200pctpov00,
    a_ov200pctpov = n_ov200pctpov19 - n_ov200pctpov00,
    # change in shares
    c_u50pctpov = s_u50pctpov19 - s_u50pctpov00,
    c_50to99pctpov = s_50to99pctpov19 - s_50to99pctpov00,
    c_upov = s_upov19 - s_upov00,
    c_u200pctpov = s_u200pctpov19 - s_u200pctpov00,
    c_ov200pctpov = s_ov200pctpov19 - s_ov200pctpov00,
    # percent changes
    p_u50pctpov = a_u50pctpov/n_u50pctpov00,
    p_50to99pctpov = a_50to99pctpov/n_50to99pctpov00,
    p_upov = a_upov/n_upov00,
    p_u200pctpov = a_u200pctpov/n_u200pctpov00,
    p_ov200pctpov = a_ov200pctpov/n_ov200pctpov00,
  ) %>%
  mutate(
    # race and ethnicity shares of tracts
    s_nlwt00 = n_nlwt00/n_totpopraceth00,
    s_nlwt19 = n_nlwt19/n_totpopraceth19,
    s_nlblk00 = n_nlblk00/n_totpopraceth00,
    s_nlblk19 = n_nlblk19/n_totpopraceth19,
    s_nlamin00 = n_nlamin00/n_totpopraceth00,
    s_nlamin19 = n_nlamin19/n_totpopraceth19,
    s_nlasi00 = n_nlasi00/n_totpopraceth00,
    s_nlasi19 = n_nlasi19/n_totpopraceth19,
    s_nlhp00 = n_nlhp00/n_totpopraceth00,
    s_nlhp19 = n_nlhp19/n_totpopraceth19,
    s_nlso00 = n_nlso00/n_totpopraceth00,
    s_nlso19 = n_nlso00/n_totpopraceth19,
    s_nl2om00 = n_nl2om00/n_totpopraceth00,
    s_nl2om19 = n_nl2om19/n_totpopraceth19,
    s_lat00 = n_lat00/n_totpopraceth00,
    s_lat19 = n_lat19/n_totpopraceth19,
    s_blklat00 = (n_nlblk00+n_lat00)/(n_totpopraceth00),
    s_blklat19 = (n_nlblk19+n_lat19)/(n_totpopraceth19),
    # absolute change - only nlwt, nlblk, nlasi, lat, and blklat
    a_nlwt = n_nlwt19 - n_nlwt00,
    a_nlblk = n_nlblk19 - n_nlblk00,
    a_nlasi = n_nlasi19 - n_nlasi00,
    a_lat = n_lat19 - n_lat00,
    a_blklat = n_blklat19 - n_blklat00,
    # change in shares
    c_nlwt = s_nlwt19 - s_nlwt00,
    c_nlblk = s_nlblk19 - s_nlblk00,
    c_nlasi = s_nlasi19 - s_nlasi00,
    c_lat = s_lat19 - s_lat00,
    c_blklat = s_blklat19 - s_blklat00,
    # percent change
    p_nlwt = a_nlwt/n_nlwt00,
    p_nlblk = a_nlblk/n_nlblk00,
    p_nlasi = a_nlasi/n_nlasi00,
    p_lat = a_lat/n_lat00,
    p_blklat = a_blklat/a_blklat) %>%
  ###### Per Capita Income
  mutate(
    n_pci00 = n_agginc00/n_totpop00,
    n_pci19 = n_agginc19/n_totpop19,
    r_n_pci00 = percent_rank(n_pci00),
    r_n_pci19 = percent_rank(n_pci19),
    a_pci = n_pci19 - n_pci00,
    x_pci00 = n_pci00/(sum(n_agginc00)/sum(n_totpop00)),
    x_pci19 = n_pci19/(sum(n_agginc19)/sum(n_totpop19)),
    # regional per capita income for each year calculated separately by dividing total regional aggregate income by total regional population
    y_pci = x_pci19 - x_pci00
  ) %>%
  ########### Overall Population and Housing Units absolute and percent changes and share of renter/owner occupied
  mutate(
    a_totpop = n_totpop19 - n_totpop00,
    a_totpoppov = n_totpoppov19 - n_totpoppov00,
    a_totpopraceth = n_totpopraceth19 - n_totpopraceth00,
    a_hu = n_hu19 - n_hu00,
    a_huten = n_huten19 - n_huten00,
    a_huown = n_huown19 - n_huown00,
    a_hurent = n_hurent19 - n_hurent00,
    s_huown00 = n_huown00/n_huten00,
    s_hurent00 = n_hurent00/n_huten00,
    s_huown19 = n_huown19/n_huten19,
    s_hurent19 = n_hurent19/n_huten19,
    p_totpoppov = a_totpoppov/n_totpoppov00,
    p_totpopraceth = a_totpopraceth/n_totpopraceth00,
    p_hu = a_hu/n_hu00,
    p_huten = a_huten/n_hu00,
    p_huown = a_huown/n_huown00,
    p_hurent = a_hurent/n_hurent00,
    c_huown = s_huown19 - s_huown00,
    c_hurent = s_hurent19 - s_hurent00) %>%
  ########### Assign categories
  mutate(nc_cats =
           case_when(
             p_ov200pctpov <= -.095 &
               c_u200pctpov >= .045 &
               a_u200pctpov <= -700 ~ "economic decline with abandonment - strong",
             p_ov200pctpov <= -.095 &
               c_u200pctpov >= .045 &
               a_u200pctpov < 0 & a_u200pctpov > -700 ~ "economic decline with abandonment - moderate",
             p_ov200pctpov <= -.095 &
               c_u200pctpov >= .045 &
               a_u200pctpov >= 700 ~ "economic decline with low-income concentration - strong",
             p_ov200pctpov <= -.095 &
               c_u200pctpov >= .045 &
               a_u200pctpov > 0 & a_u200pctpov < 700 ~ "economic decline with low-income concentration - moderate",
             p_ov200pctpov >= .095 &
               c_u200pctpov <= -.045 &
               a_u200pctpov <= -700 ~ "economic expansion with displacement - strong",
             p_ov200pctpov >= .095 &
               c_u200pctpov <= -.045 &
               a_u200pctpov < 0 & a_u200pctpov > -700 ~ "economic expansion with displacement - moderate",
             p_ov200pctpov >= .095 &
               c_u200pctpov <= -.045 &
               a_u200pctpov > 0 & a_u200pctpov < 700 ~ "economic expansion with overall growth - moderate",
             p_ov200pctpov >= .095 &
               #pct change middle high income population >= 10%
               c_u200pctpov <= -.045 &
               #change in low income share <= 5%
               a_u200pctpov >= 700
             # number of low income people grew by more than 700
             ~ "economic expansion with overall growth - strong",
             TRUE ~ "other"
           )) %>%
  # broader nc categories - without moderate/strong subcategories
  mutate(nc_cats_br =
           case_when(
             p_ov200pctpov <= -.095 &
               c_u200pctpov >= .045 &
               a_u200pctpov < 0 ~ "economic decline with abandonment",
             p_ov200pctpov <= -.095 &
               c_u200pctpov >= .045 &
               a_u200pctpov > 0 ~ "economic decline with low income concentration",
             p_ov200pctpov >= .095 &
               c_u200pctpov <= -.045 &
               a_u200pctpov < 0 ~ "economic expansion with displacement",
             p_ov200pctpov >= .095 &
               c_u200pctpov <= -.045 &
               a_u200pctpov > 0 ~ "economic expansion with overall growth",
             TRUE ~ "other"
           )) %>%
  # per capita income change categories, not all used in report
  mutate(catA_pci =
           case_when(
             y_pci <= -.10 ~ "decline 10% or more",
             y_pci >= .10 ~ "increase 10% or greater",
             TRUE ~ "lt 10% change"
           )) %>%
  mutate(catB_pci =
           case_when(r_n_pci00 >= .7 & r_n_pci19 >= .7 ~ "stayed 70% or over regional pci",
                     r_n_pci00 <= .3 & r_n_pci19 <= .3 ~ "stayed 30% or under regional pci",
                     TRUE ~ "stayed between 30% & 70% regional pci")) %>%
  mutate(catC_pci =
           case_when(r_n_pci00 <= .4 & r_n_pci19 <= .4 ~ "stayed below 40th pctile of per capita income",
                     r_n_pci00 >= .6 & r_n_pci19 >= .6 ~ "stayed above 60th pctile of per capita income",
                     r_n_pci00 > .4 & r_n_pci00 <= .6 & r_n_pci19 > .4 & r_n_pci19 <= .6 ~ "statyed between 40th and 60th percentile of per capita income",
                     TRUE ~ "other"))

# write dataframe into csv for future use if desired
# write_csv(neighb_chg_base_derived_vars, "your_location/your_filename.csv")

# create shapefile of neighborhood change data
library("sf")
library("tmap")
# national file of tracts for quick download
#tr_shp <- tigris::tracts(cb = TRUE, class = "sf")
#neighb_chg_final_shp <- tr_shp %>%
#  inner_join(neighb_chg_base_derived_vars, by = c("GEOID" = "geoid10"))

# download shapefiles for states with tracts in the DC metro area
tr_shp_dc <- tigris::tracts(cb = TRUE, class = "sf", state = 11, year = 2019)
tr_shp_md <- tigris::tracts(cb = TRUE, class = "sf", state = 24, year = 2019)
tr_shp_va <- tigris::tracts(cb = TRUE, class = "sf", state = 51, year = 2019)
tr_shp_wv <- tigris::tracts(cb = TRUE, class = "sf", state = 53, year = 2019)

# bind state shapefiles together
tr_shp <- tr_shp_dc %>%
  bind_rows(tr_shp_md) %>%
  bind_rows(tr_shp_va) %>%
  bind_rows(tr_shp_wv) %>%
  mutate(geoid10 = GEOID)

# join geographic data to neighborhood change data
neighb_chg_final_shp <- tr_shp %>%
  right_join(neighb_chg_base_derived_vars, by = "geoid10")

# write shapefile
st_write(neighb_chg_final_shp, "your_location/your_filename.shp")