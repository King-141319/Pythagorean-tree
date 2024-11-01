import Graphics.Gloss
import Graphics.Gloss.Data.Vector
  
-- Main function to display the tree
main :: IO ()
main = display (InWindow "Pythagorean Tree" (800, 800) (10, 10)) white (pictures [color red $ circle 2, polygon[(0,0),(0,50),(50,50),(50,0)], drawTree 10 (0,50) (50,50) 53.13])

-- Function to draw the tree fractal
drawTree :: Int -> Point -> Point -> Float -> Picture
drawTree 0 _ _ _ = Blank  -- Base case: no more recursion when depth is 0
drawTree depth (xa , ya) (xb , yb) angle =
    pictures[ 
      color blue $ polygon[
            (xa , ya),
            (xa + lddx, ya + lddy),
            (xa + ldx + lddx , ya + ldy + lddy),
            (xa + ldx , ya + ldy)
            ]
      color red $ polygon[
            (xb , yb),
            (xb + rddx , yb + rddy),
            (xb + rdx + rddx , yb + rdy + rddy),
            (xb + rdx , yb + rdy)
            ],
      drawTree (depth - 1) (xa + ldx, ya + ldy) (xa + ldx + lddx , ya + ldy + lddy) angle,
      drawTree (depth - 1) (xb + rdx + rddx , yb + rdy + rddy) (xb + rdx, yb + rdy) angle]

      where
            (ldx , ldy) = rotateV (0.5 * pi) (lddx, lddy)
            (lddx , lddy) = rotateV (angle / 180 * pi) ((cos(angle / 180 * pi))*(xb-xa),(cos(angle / 180 * pi))*(yb-ya))
            (rdx , rdy) = rotateV (-0.5 * pi) (rddx, rddy)
            (rddx , rddy) = rotateV (angle / 180 * pi - 0.5 * pi) ((sin(angle / 180 * pi)) * (xa - xb) , (sin(angle / 180 * pi)) * (ya - yb))
    

