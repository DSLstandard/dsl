module DSL.WeakenTm where

import DSL.TravTm
import DSL.Types
import Relude

weakenTm :: TmLvl -> Tm -> Tm
weakenTm pad =
  if pad == 0
    then
      -- NOTE: We do rely on this optimization in some places, so we want to
      -- make sure it actually happens.
      Relude.id
    else
      mapTm
        ( \lvl tm def -> do
            case tm of
              TmVar i ->
                if i < lvl
                  then TmVar i
                  else TmVar (i + pad)
              _ -> def
        )