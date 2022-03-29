###HOMEWORK 4###

library(tidyverse)
library(rvest)
library(purrr)
library(polite)

session = bow("https://www.ncleg.gov/Members/MemberList/S")

resp = scrape(session)

senator_elements = html_elements(resp, ".col-8")

# The following will scrape the website and create a dataset ('senators') that will contain
# the name, district, and NC Legislature url for each state senator.

senators = map_dfr(senator_elements, function(element) {
  senator_name = html_element(element, "a") %>% html_text2()
  senator_district = html_element(element, "p:contains(District)") %>% html_text2()
  senator_url = html_element(element, "a") %>% html_attr("href")
  
  return(list(
    "name"=senator_name,
    "DISTRICT"=senator_district,
    "url"=senator_url
  ))
})

# Despite there being 50 senators in the North Carolina senate, there are 51 observations. This
# is the result of Sen. Sam Searcy resigning in 2020 to serve on the North Carolina Board
# of Community Colleges and being replaced by Sen. Sydney Batch. The following code will
# remove the extra row of unnecessary data.

senators = senators[-c(48), ]

# The following code will convert 'senator$DISTRICT' to having just the senator's district
# number as just the district number, without the "District".

senators$DISTRICT = str_extract(senators$DISTRICT, "[0-9]+$")

# In order to determine the number of terms each member has served, we have to 
# extract data from their individual biographies. Using the information 
# from above, we have have the urls for each senator's biography, but 
# we need to add 'https://ncleg.gov' to the beginning of each to make them
# useful for the purposes of this assignment.

senators$url = paste0("https://www.ncleg.gov", senators$url)

# Now we create a new session to harvest the data we need from the 
# senators' biographies.

terms_sess = bow("https://www.ncleg.gov")

# Now, we use this function to extract the term data from each senator's 
# individual biography.

get_terms = function(url) {
  terms_sess <<- nod(terms_sess, url)
  resp = scrape(terms_sess)
  terms = html_elements(resp, ".col-12 p:contains('in House')") %>%
    first() %>%
    html_text2()
  
  return(terms)
}

# And we use these functions to create a new dataset containing the number
# of terms served by each senator.

with_terms = rowwise(senators, everything()) %>%
  mutate(terms=get_terms(url))

# We can use a virtually identical function to harvest data containing
# the senators' partisan affiliations. I had some difficulty just 
# grabbing the partisan affiliation from the directory, but I figured
# out a workaround using data from the senators' biographies.

party_sess = bow("https://www.ncleg.gov")

get_party = function(url) {
  party_sess <<- nod(party_sess, url)
  resp = scrape(party_sess)
  party = html_elements(resp, "h6:contains('District')") %>%
    first() %>%
    html_text2()
  
  return(party)
}

with_party = rowwise(with_terms, everything()) %>%
  mutate(party=get_party(url))

# We now have a complete dataset, containing the senator's name,
# district, website, partisan affiliation and number of terms served.
# However, the data still needs to be cleaned up. The 'party' column
# still contains more info than the senator's partisan affiliation,
# and the terms column contains unnecessary characters and the number
# of terms the senator served in the House of Representatives.

# First we will address the number of terms. By using code similar to that
# in line 38, we can extract the number of senate terms each senator has served.
# In this case, however, we want the number from the beginning of the string,
# not anywhere else.

with_party$terms = as.numeric(str_extract(with_party$terms, "^[0-9]+"))

# One senator, Sen Sydney Batch, has an NA for her column has she has yet to serve
# a full term. On her biography page, her number of senate terms simply read '+'. This 
# '+' showed up in a number of senators' profiles and I'm assuming that means that the 
# senator has served a partial term in addition to their number of full terms.
# For the purposes of this assignment, I will only count full terms, and will therefore
# remove Sen. Batch from this data. 

with_party = with_party[-c(4), ]

# I will also remove the 'url' column, as it is no longer necessary.

with_party = with_party[-c(3)]

# Lastly, we need to clean up the 'party' column. We can do this by extracting the information
# that is related to the senators' partisan affiliation by using a regular expression.

with_party$party = str_extract(with_party$party, "^(Democrat|Republican)")

# Now we have a complete, neat table containing the senators' names, districts, number
# of terms served, and their partisan affiliations.

View(with_party)

# Finally, we can save this dataset as a csv.

write_csv(with_party, "nc_senate_composition_2022.csv")

# Next, we can calculate whether or not Democratic or Republican senators have
# served more terms, on average using a the group_by function. 

group_by(with_party, party) %>%
  summarize(mean_terms=mean(terms))

# According to the data, Republicans have a served a slightly higher 3.18 terms
# compared to the Democrats average of 3 terms. 

# Now we can use mapping functions to show a) which districts are represented by
# Republicans and b) how many terms senators in each district have served. 

# First, we'll need to load the 'sf' library so we can do some mapping.

library(sf)

# Then, we need to read the 2019 NC Senate district map (the ones used in the 2020 election) 
# shapefile.

senate2019 = read_sf("Senate Consensus Nonpartisan Map v3.shp")

# We now join 'senate2019' and 'with_party' by district, so we can use the data
# from both to make our maps.

senate2019 = left_join(senate2019, with_party, on="DISTRICT")

# Now, we can map out which districts are held by Republicans.

senateGOP = filter(senate2019, party == "Republican")

# The map below shows which seats are held by Republicans and which are held
# by Democrats. Note: There is one seat marked N/A; this is District 17,
# the district served by the previously mentioned Sens. Sam Searcy and 
# Sydney Batch.

ggplot() +
  geom_sf(data=senateGOP, aes(fill=party))

# We can perform a similar function to find out the number of terms
# served by the senator representing each district.

ggplot() +
  geom_sf(data=senate2019, aes(fill=terms), lwd=0) +
  scale_fill_fermenter(palette="Reds", n.breaks=8)
