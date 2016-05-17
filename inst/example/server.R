shinyServer(function(input, output, session) {

	# source shared functions
	source(file.path(r_path,"radiant.data/init.R"), encoding = r_encoding, local = TRUE)
	source(file.path(r_path,"radiant.data/radiant.R"), encoding = r_encoding, local = TRUE)

	# source data & app tools from base
	for (file in list.files(c(file.path(r_path,"radiant.data/tools/app"),
	    									 file.path(r_path,"radiant.data/tools/data")),
												 pattern="\\.(r|R)$", full.names = TRUE))
	  source(file, encoding = r_encoding, local = TRUE)

	# source additional analysis tools for example app
  for (file in list.files(c("tools/"),
      									 pattern="\\.(r|R)$", full.names = TRUE))
	  source(file, encoding = r_encoding, local = TRUE)

  # save state on refresh or browser close
  saveStateOnRefresh(session)

})
