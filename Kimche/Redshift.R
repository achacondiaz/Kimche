#Install RJDBC package
dir.create('~/.redshiftTools')
download.file('http://s3.amazonaws.com/redshift-downloads/drivers/RedshiftJDBC41-1.1.9.1009.jar','~/.redshiftTools/redshift-driver.jar')
install.packages('RJDBC')

install.packages("RPostgres")

#RS conn
pconn_r <- dbConnect(RPostgres::Postgres(),
                     host = host,
                     port = port,
                     user = user,
                     password = password,
                     dbname = dbname,
                     sslmode='require')