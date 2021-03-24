module Admin.Job.Test where
import Admin.Controller.Prelude

instance Job TestJob where
    perform TestJob { .. } = do
        putStrLn "Hello World!"
