module DSL.Name where

import Relude

-- Separated into a single module to untie circular dependencies between Core
-- and Syntax.
type Name = Text