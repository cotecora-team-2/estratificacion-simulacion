library('pins')
library('dotenv')
# Load from a file
dotenv_file <- '.env'
path <- './datos'
load_dot_env(dotenv_file)

board_register_github(name="mygithub", repo = "cotecora-team-2/quickcountmx-data", branch = 'main', token=Sys.getenv("token"))

sims <- pin_get('tiempos_simulaciones', board='mygithub')
readr::write_rds(sims, paste(path,"simulaciones_completo.rds",sep="/"))
rm(sims)
