library('pins')
library('dotenv')
# Load from a file
dotenv_file <- '.env'
load_dot_env(dotenv_file)

board_register_github(name="mygithub", repo = "cotecora-team-2/quickcountmx-data", branch = 'main', token=Sys.getenv("token"))

sims <- pin_get('tiempos_simulaciones', board='mygithub')
readr::write_rds(sims,file = "./datos/simulaciones_completo.rds")
rm(sims)
