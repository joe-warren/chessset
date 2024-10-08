{-# LANGUAGE RecordWildCards #-}
module Knight 
( topper
) where
import qualified Waterfall
import Linear (V2 (..), V3 (..), zero, _x, _z, _xy)
import Data.Maybe (fromMaybe)
import Data.Function ((&))
import Control.Lens ((.~), (^.))
import qualified Topper

-- "m 9.3619051,18.867762 
--  h 6.2763869 
-- c 1.561074,-2.70386 3.289859,-5.798287 3.289859,-7.544921 0,-1.7466336 -0.307008,-6.3068706 -6.018196,-6.3068706 
-- l -1.646878,-2.027632 c 0,0 -0.591067,0.44089 -0.83343,0.84231 -0.242363,0.40142 -0.23622,1.485939 -0.23622,1.485939 -1.4680139,1.04377 -2.2279639,1.842628 -2.3302259,3.154448 -0.551165,1.092593 -2.3300428,3.7009306 -2.5533998,4.1535606 -0.223357,0.45263 -0.427143,0.952468 -0.02003,1.546262 0.407113,0.593794 1.746622,1.085316 2.5995968,0.892446 0.852975,-0.192871 1.722425,-1.701847 1.722425,-1.701847 1.4787809,0.264696 2.6319639,-0.791476 3.7814759,-1.862541 0.626774,-0.584 1.057426,-1.5084196 1.057426,-1.5084196 0,1.6382376 -0.460706,3.0262286 -0.854531,3.4860246 -1.007864,1.176694 -2.560838,2.447754 -3.07325,2.960166 -0.512412,0.512412 -1.1610089,1.539705 -1.1610089,2.431075 z"

xSectionC :: Waterfall.Path2D
xSectionC = Waterfall.pathFrom (V2 9.3619051 18.867762)
    [ Waterfall.lineRelative (V2 6.2763869 0)
    , Waterfall.bezierRelative (V2 1.561074 (-2.70386)) (V2 3.289859 (-5.798287)) (V2 3.289859 (-7.544921)) 
    , Waterfall.bezierRelative (V2 0 (-1.7466336)) (V2 (-0.307008) (-6.3068706)) (V2 (-6.018196) (-6.3068706)) 
    , Waterfall.lineRelative (V2 (-1.646878) (-2.027632))
    , Waterfall.bezierRelative (V2 0 0) (V2 (-0.591067) 0.44089) (V2 (-0.83343) 0.84231)
    , Waterfall.bezierRelative (V2 (-0.242363) 0.40142) (V2 (-0.23622) 1.485939) (V2 (-0.23622) 1.485939)
    , Waterfall.bezierRelative (V2 (-1.4680139) 1.04377) (V2 (-2.2279639) 1.842628) (V2 (-2.3302259) 3.154448)
    , Waterfall.bezierRelative (V2 (-0.551165) 1.092593) (V2 (-2.3300428) 3.7009306) (V2 (-2.5533998) 4.1535606)
    , Waterfall.bezierRelative (V2 (-0.223357) 0.45263) (V2 (-0.427143) 0.952468) (V2 (-0.02003) 1.546262)
    , Waterfall.bezierRelative (V2 0.407113 0.593794) (V2 1.746622 1.085316) (V2 2.5995968 0.892446)
    , Waterfall.bezierRelative (V2 0.852975 (-0.192871)) (V2 1.722425 (-1.701847)) (V2 1.722425 (-1.701847)) 
    , Waterfall.bezierRelative (V2 1.4787809 0.264696) (V2 2.6319639 (-0.791476)) (V2 3.7814759 (-1.862541))
    , Waterfall.bezierRelative (V2 0.626774 (-0.584)) (V2 1.057426 (-1.5084196)) (V2 1.057426 (-1.5084196))
    , Waterfall.bezierRelative (V2 0 1.6382376) (V2 (-0.460706) 3.0262286) (V2 (-0.854531) 3.4860246)
    , Waterfall.bezierRelative (V2 (-1.007864) 1.176694) (V2 (-2.560838) 2.447754) (V2 (-3.07325) 2.960166)
    , Waterfall.bezierRelative (V2 (-0.512412) 0.512412) (V2 (-1.1610089) 1.539705) (V2 (-1.1610089) 2.431075)
    ]

 
-- "M 9.3619051,18.867762 
-- H 19.745604 
-- c 0.890683,-3.324073 1.092169,-3.901989 1.092169,-6.116344 0,-2.214355 -2.21663,-7.7354476 -7.927818,-7.7354476 
-- l -1.646878,-2.027632 
-- c 0,0 -0.591067,0.44089 -0.83343,0.84231 -0.242363,0.40142 -0.23622,1.485939 -0.23622,1.485939 -1.4680139,1.04377 -2.2279639,1.842628 -2.3302259,3.154448 -0.551165,1.092593 -2.3300428,3.7009306 -2.5533998,4.1535606 -0.223357,0.45263 -0.427143,0.952468 -0.02003,1.546262 0.407113,0.593794 1.746622,1.085316 2.5995968,0.892446 0.852975,-0.192871 1.722425,-1.701847 1.722425,-1.701847 1.4787809,0.264696 2.6319639,-0.791476 3.7814759,-1.862541 0,0 0.646952,1.208475 0.202895,1.977605 -0.444057,0.76913 -2.560838,2.447754 -3.07325,2.960166 -0.512412,0.512412 -1.1610089,1.539705 -1.1610089,2.431075 z"

xSectionB :: Waterfall.Path2D
xSectionB = Waterfall.pathFrom (V2 9.3619051 18.867762)
    [ Waterfall.lineTo (V2 19.745604 18.867762) 
    , Waterfall.bezierRelative (V2 0.890683 (-3.324073)) (V2 1.092169 (-3.901989)) (V2 1.092169 (-6.116344))
    , Waterfall.bezierRelative (V2 0 (-2.214355)) (V2 (-2.21663) (-7.7354476)) (V2 (-7.927818) (-7.7354476))
    , Waterfall.lineRelative (V2 (-1.646878) (-2.027632))
    , Waterfall.bezierRelative (V2 0 0) (V2 (-0.591067) 0.44089) (V2 (-0.83343) 0.84231)
    , Waterfall.bezierRelative (V2 (-0.242363) 0.40142) (V2 (-0.23622) 1.485939) (V2 (-0.23622) 1.485939)
    , Waterfall.bezierRelative (V2 (-1.4680139) 1.04377) (V2 (-2.2279639) 1.842628) (V2 (-2.3302259) 3.154448)
    , Waterfall.bezierRelative (V2 (-0.551165) 1.092593) (V2 (-2.3300428) 3.7009306) (V2 (-2.5533998) 4.1535606)
    , Waterfall.bezierRelative (V2 (-0.223357) 0.45263) (V2 (-0.427143) 0.952468) (V2 (-0.02003) 1.546262)
    , Waterfall.bezierRelative (V2 0.407113 0.593794) (V2 1.746622 1.085316) (V2 2.5995968 0.892446)
    , Waterfall.bezierRelative (V2 0.852975 (-0.192871)) (V2 1.722425 (-1.701847)) (V2 1.722425 (-1.701847))
    , Waterfall.bezierRelative (V2 1.4787809 0.264696) (V2 2.6319639 (-0.791476)) (V2 3.7814759 (-1.862541))
    , Waterfall.bezierRelative (V2 0 0) (V2 0.646952 1.208475) (V2 0.202895 1.977605)
    , Waterfall.bezierRelative (V2 (-0.444057) 0.76913) (V2 (-2.560838) 2.447754) (V2 (-3.07325) 2.960166)
    , Waterfall.bezierRelative (V2 (-0.512412) 0.512412) (V2 (-1.1610089) 1.539705) (V2 (-1.1610089) 2.431075)
    ]


xSectionA :: Waterfall.Path2D
xSectionA = Waterfall.pathFrom (V2 9.3619051 18.867762)
    [ Waterfall.lineTo (V2 19.745604 18.867762) 
    , Waterfall.bezierRelative (V2 0.890683 (-3.324073)) (V2 1.092169 (-3.901989)) (V2 1.092169 (-6.116344))
    , Waterfall.bezierRelative (V2 0 (-2.214355)) (V2 (-2.21663) (-7.7354476)) (V2 (-7.927818) (-7.7354476))
    , Waterfall.lineRelative (V2 (-2.716528) 0.3006169999999997)
    , Waterfall.bezierRelative (V2 (-1.4680139) 1.04377) (V2 (-2.2279639) 1.842628) (V2 (-2.3302259) 3.154448)
    , Waterfall.bezierRelative (V2 (-0.551165) 1.092593) (V2 (-2.3300428) 3.7009306) (V2 (-2.5533998) 4.1535606)
    , Waterfall.bezierRelative (V2 (-0.223357) 0.45263) (V2 (-0.427143) 0.952468) (V2 (-0.02003) 1.546262)
    , Waterfall.bezierRelative (V2 0.407113 0.593794) (V2 1.746622 1.085316) (V2 2.5995968 0.892446)
    , Waterfall.bezierRelative (V2 0.852975 (-0.192871)) (V2 1.722425 (-1.701847)) (V2 1.722425 (-1.701847))
    , Waterfall.bezierRelative (V2 1.4787809 0.264696) (V2 2.6319639 (-0.791476)) (V2 3.7814759 (-1.862541))
    , Waterfall.bezierRelative (V2 0 0) (V2 0.646952 1.208475) (V2 0.202895 1.977605)
    , Waterfall.bezierRelative (V2 (-0.444057) 0.76913) (V2 (-2.560838) 2.447754) (V2 (-3.07325) 2.960166)
    , Waterfall.bezierRelative (V2 (-0.512412) 0.512412) (V2 (-1.1610089) 1.539705) (V2 (-1.1610089) 2.431075)
    ]

-- "m 10.111197,8.4484604 
-- c 0,0 0.535902,-0.942478 1.00579,-1.00579 0.3327,-0.04483 0.875229,1.7e-4 0.875229,0.498261 0,0.433209 -0.275165,0.529362 -0.650193,0.529362 -0.410339,0 -1.230826,-0.02183 -1.230826,-0.02183 z"

xSectionEye :: Waterfall.Path2D
xSectionEye = Waterfall.pathFrom (V2 10.111197 8.4484604) 
    [ Waterfall.bezierRelative (V2 0 0) (V2 0.535902 (-0.942478)) (V2 1.00579 (-1.00579))
    , Waterfall.bezierRelative (V2 0.3327 (-0.04483)) (V2 0.875229 1.7e-4) (V2 0.875229 0.498261)
    , Waterfall.bezierRelative (V2 0 0.433209) (V2 (-0.275165) (0.529362)) (V2 (-0.650193) 0.529362)
    , Waterfall.bezierRelative (V2 (-0.410339) 0) (V2 (-1.230826) (-0.02183)) (V2 (-1.230826) (-0.02183))
    ]

topper :: Double -> Topper.Args -> Waterfall.Solid
topper r _ = 
    let p x t = Waterfall.translate (V3 0 0 x) . Waterfall.prism t . Waterfall.fromPath
        eyeCutA = p (-1) 1.5 xSectionEye 
        eyeCutB = p 4.5 1.5 xSectionEye 
        withEye = (`Waterfall.difference` (eyeCutA <> eyeCutB))
        position x = 
            let rx = Waterfall.rotate (V3 1 0 0) (-pi/2) x
                (br, tl) = fromMaybe (error "bad aabb") $ Waterfall.axisAlignedBoundingBox rx 
                target = zero & _xy .~ ((br ^. _xy + tl ^. _xy) / 2) & _z .~ (br ^. _z)
                s = 2.4 * r / ((tl - br) ^. _x)
             in Waterfall.translate (V3 (-0.2 * r) 0 (-0.01)) 
                    . Waterfall.uScale s 
                    . Waterfall.translate (negate target) 
                    $ rx
    in 
        position . withEye $ 
            p 0 0.5 xSectionC <>
            p 0.5 0.5 xSectionB <>
            p 1 3 xSectionA <>
            p 4 0.5 xSectionB <>
            p 4.5 0.5 xSectionC

