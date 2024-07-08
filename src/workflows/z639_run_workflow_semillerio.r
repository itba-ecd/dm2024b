#  NO INCLUYE  conclusiones de Experimentos Colaborativos
#  usted ya sabe que conviene entrenar en mucho mas que 9 meses
#  NO hace el submit automatico a Kaggle, usted lo debera hacer a mano

require("rlang")

envg <- env()

envg$EXPENV <- list()
envg$EXPENV$repo_dir <- "~/dm2024b/"

#------------------------------------------------------------------------------

correr_workflow <- function( wf_scriptname )
{
  # creo el script que corre el experimento
  comando <- paste0( "nice -n 15 Rscript --vanilla ",
                     envg$EXPENV$repo_dir,
                     wf_scriptname,
                     "   ",
                     wf_scriptname
                   )

  system( comando )
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

# aqui efectivamente llamo al workflow
correr_workflow( "src/workflows/z639_workflow_semillerio_f202109.r" )
