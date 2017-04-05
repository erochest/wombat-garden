
{-# LANGUAGE LambdaCase #-}


module Opts
    ( Actions(..)
    , opts
    , execParser
    , parseOpts
    ) where


import           Data.Monoid
import           Options.Applicative
import           Options.Applicative.Text

import           Types
import           WombatGarden.Types


paramsOpts :: Parser Parameters
paramsOpts
  =   Parameters
  <$> option auto (  short 'p' <> long "population" <> value 50
                  <> metavar "POPULATION_SIZE"
                  <> help "The size of the population.\
                          \ Default is 50. ")
  <*> option auto (  long "new-name" <> value 0.33
                  <> metavar "NEW_NAME_PERCENTAGE"
                  <> help "The chance that the system will create\
                          \ a new name instead of re-using an\
                          \ existing one.")
  <*> option auto (  short 'c' <> long "carry-over" <> value 5
                  <> metavar "CARRYOVER_SIZE"
                  <> help "The number of members to\
                          \ carry-over directly into the next\
                          \ generation. Default is 5.")
  <*> option auto (  short 'N' <> long "new-random" <> value 3
                  <> metavar "NEW_RANDOM_SIZE"
                  <> help "The number of new members to\
                          \ generate randomly each generation.\
                          \ Default is 3.")
  <*> option auto (  short 'x' <> long "crossover-rate"
                  <> metavar "CROSSOVER_RATE" <> value 0.01
                  <> help "The rate at which to perform\
                          \ crossovers. Default is 0.01.")
  <*> option auto (  short 'm' <> long "mutation-rate"
                  <> metavar "MUTATION_RATE" <> value 0.01
                  <> help "The rate at which to perform\
                          \ mutation. Default is 0.01.")
  <*> option auto (  short 'd' <> long "depth" <> value 20
                  <> metavar "DEPTH"
                  <> help "How deep to let the program's\
                          \ structure grow to? Default is 20.")

initOpts :: Parser Actions
initOpts =   Init
         <$> textOption ( short 'n' <> long "name"
                        <> metavar "GARDEN_NAME"
                        <> help "The name of the garden. This will\
                                \ be used for the name of the\
                                \ github repository. This will be\
                                \ created under the user account\
                                \ that you have set up for\
                                \ passwordless login from the command\
                                \ line.")
         <*> paramsOpts
         <*> switch (  short 'P' <> long "publish"
                    <> help "Publish this repository to Github?")

growOpts :: Parser Actions
growOpts
  =   Grow
  <$> textOption (  short 'n' <> long "name" <> metavar "GARDEN_NAME"
                 <> help "The name of the garden/Github repo.")
  <*> textOption (  short 'k' <> long "key" <> metavar "SERVER_KEY"
                 <> help "The access key to the server. You can get\
                         \ this by logging into the web client and\
                         \ getting the key from local storage.")
  <*> optional (option auto
               (  short 'n' <> long "generations"
                 <> metavar "GENERATIONS"
                 <> help "The number of generations to run. If not\
                         \ given, run until killed."))
  <*> strOption (  short 'u' <> long "api-url" <> metavar "API_URL"
                <> help "The URL for the API server.")

reportOpts :: Parser Actions
reportOpts
  =   Report
  <$> textOption (  short 'n' <> long "name" <> metavar "GARDEN_NAME"
                 <> help "The name of the garden/Github repo.")
  <*> strOption (  short 'o' <> long "output" <> metavar "OUTPUT"
                <> help "The file to write the report to.")

opts' :: Parser Actions
opts' = subparser
        (  command "init" (info (helper <*> initOpts)
                            (progDesc "Initialize the garden/Github\
                                      \ repo."))
        <> command "grow" (info (helper <*> growOpts)
                           (progDesc "Run the GP for a period of\
                                     \ time."))
        <> command "report" (info (helper <*> reportOpts)
                            (progDesc "Report on the history and\
                                      \ current state of the garden."))
        )

opts :: ParserInfo Actions
opts = info (helper <*> opts')
       (  fullDesc
       <> progDesc "This is a genetic programming system to\
                   \ cultivate warrior wombat-bots\
                   \ (http://wombats.io)."
       <> header "wombat-garden - A garden to grow Wombat-bots.")

parseOpts :: IO Actions
parseOpts = execParser opts
