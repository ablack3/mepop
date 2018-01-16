df <- read.csv(text = "
state, fips, county, h1
ME,23001,Androscoggin County,H1
ME,23003,Aroostook County,H1
ME,23005,Cumberland County,H1
ME,23007,Franklin County,H1
ME,23009,Hancock County,H1
ME,23011,Kennebec County,H1
ME,23013,Knox County,H1
ME,23015,Lincoln County,H1
ME,23017,Oxford County,H1
ME,23019,Penobscot County,H1
ME,23021,Piscataquis County,H1
ME,23023,Sagadahoc County,H1
ME,23025,Somerset County,H1
ME,23027,Waldo County,H1
ME,23029,Washington County,H1
ME,23031,York County,H1")

write.csv(df[,c(-1,-4)],"county_fips.csv", row.names = F)
