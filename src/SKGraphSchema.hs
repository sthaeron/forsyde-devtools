{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

module SKGraphSchema where

import Data.Aeson ((.=))
import qualified Data.Aeson as A
import qualified Data.Map as M
import qualified Data.Sequence as Seq
import qualified Data.Text as T

-- | Structural graph elements. The Id should be unique
data GraphElement
  = KLabel
      { label :: !T.Text,
        gid :: !T.Text
      }
  | KNode
      { children :: ![GraphElement],
        renderings :: ![KRendering],
        properties :: !KProperties,
        gid :: !T.Text
      }
  | KPort
      { children :: ![GraphElement],
        renderings :: ![KRendering],
        properties :: !KProperties,
        gid :: !T.Text
      }
  | KEdge
      { children :: ![GraphElement],
        renderings :: ![KRendering],
        properties :: !KProperties,
        gid :: !T.Text,
        source :: !T.Text,
        target :: !T.Text
      }
  | KGraph
      { child :: !GraphElement,
        properties :: !KProperties,
        gid :: !T.Text
      }

-- | ELK properties that influence layout, rendering, etc
data KProperty
  = NodeLabelsPlacement [Int]
  | NodeSizeConstraints [Int]
  | NodeSizeMinimum [Int]
  | EdgeType Int
  | JunctionPoints [Int]
  | LayerConstraint Int
  | PortBorderOffset Float
  | PortSide Int
  | PortConstraints Int

data KPlacementData
  = KTopPosition Float Float -- absolute (Float), relative (Float)
  | KLeftPosition Float Float -- absolute (Float), relative (Float)
  | KRightPosition Float Float -- absolute (Float), relative (Float)
  | KBottomPosition Float Float -- absolute (Float), relative (Float)

-- "Fake" KRendering called "KArrow". A function that returns a KPolygon that
-- creates an arrow. KLineJoin rounds the edges on the arrow
kArrow :: KRendering
kArrow =
  KPolygon
    [KBackgroundColor 0 0 0, KLineJoin, KLineWidth]
    [ (KLeftPosition 0.0 0.0, KTopPosition 0.0 0.0),
      (KLeftPosition 0.0 0.4, KTopPosition 0.0 0.5),
      (KLeftPosition 0.0 0.0, KBottomPosition 0.0 0.0),
      (KRightPosition 0.0 0.0, KBottomPosition 0.0 0.5)
    ]

-- We might want to change this later if we add the symbolic enum names
type KProperties = [KProperty]

-- | Styles which change how an object is rendered
data KStyle
  = KBackgroundColor Int Int Int
  | KForegroundColor Int Int Int
  | KLineJoin
  | KLineWidth

instance A.ToJSON KStyle where
  toJSON style = case style of
    KBackgroundColor r g b -> color "KBackgroundImpl" r g b
    KForegroundColor r g b -> color "KForegroundImpl" r g b
    KLineJoin ->
      A.object
        [ "lineJoin" .= (1 :: Int),
          "miterLimit" .= (10 :: Int),
          "type" .= T.pack "KLineJoinImpl",
          "propagateToChildren" .= (False :: Bool),
          "selection" .= (False :: Bool)
        ]
    KLineWidth ->
      A.object
        [ "lineWidth" .= (1 :: Int),
          "type" .= T.pack "KLineWidthImpl",
          "propagateToChildren" .= (False :: Bool),
          "selection" .= (False :: Bool)
        ]
    where
      -- Note that these types support more properties but they are not relevant
      -- to our project at the moment
      color t r g b =
        A.object
          [ "type" .= (T.pack t),
            "color"
              .= A.object
                [ "red" .= r,
                  "green" .= g,
                  "blue" .= b
                ],
            "alpha" .= (255 :: Int),
            "selection" .= False
          ]

-- | Sprotty renderings which are what is actually displayed
data KRendering
  = KEllipse [KStyle]
  | KPolyline [KStyle]
  | KRoundedBendsPolyline [KStyle] Float -- bendRadius (Float)
  | KArc [KStyle] Float Float -- startAngle (Float), arcAngle (Float)
  | KSpline [KStyle]
  | KRectangle [KStyle]
  | KRoundedRectangle [KStyle] Float Float -- cornerWidth (Float), cornerHeight (Float)
  | KText T.Text [KStyle]
  | KPolygon [KStyle] [(KPlacementData, KPlacementData)] -- points, list (x,y) [(KPlacementData, KPlacementData)]

buildCoords :: [(KPlacementData, KPlacementData)] -> [A.Value]
buildCoords placements = case placements of
  (p1, p2) : xs ->
    A.object
      [ "x" .= p1,
        "y" .= p2
      ]
      : buildCoords xs
  [] -> []

instance A.ToJSON KPlacementData where
  toJSON placement = case placement of
    KTopPosition absolute relative -> simple "KTopPositionImpl" absolute relative
    KLeftPosition absolute relative -> simple "KLeftPositionImpl" absolute relative
    KRightPosition absolute relative -> simple "KRightPositionImpl" absolute relative
    KBottomPosition absolute relative -> simple "KBottomPositionImpl" absolute relative
    where
      simple name absolute relative =
        A.object
          [ "type" .= T.pack name,
            "absolute" .= absolute,
            "relative" .= relative
          ]

instance A.ToJSON KRendering where
  toJSON rendering = case rendering of
    KEllipse styles -> simple "KEllipseImpl" styles
    KPolyline styles -> simple "KPolylineImpl" styles
    KRoundedBendsPolyline styles radius ->
      A.object
        [ "type" .= T.pack "KRoundedBendsPolylineImpl",
          "children" .= (Seq.empty :: Seq.Seq A.Object),
          "actions" .= (Seq.empty :: Seq.Seq A.Object),
          "styles" .= styles,
          "bendRadius" .= radius,
          "properties"
            .= A.object
              [ "klighd.lsp.rendering.id" .= T.pack "$R0"
              ]
        ]
    KArc styles startAngle arcAngle ->
      A.object
        [ "type" .= T.pack "KArcImpl",
          "children" .= (Seq.empty :: Seq.Seq A.Object),
          "actions" .= (Seq.empty :: Seq.Seq A.Object),
          "styles" .= styles,
          "startAngle" .= startAngle,
          "arcAngle" .= arcAngle,
          "properties"
            .= A.object
              [ "klighd.lsp.rendering.id" .= T.pack "$R0"
              ]
        ]
    KRoundedRectangle styles w h ->
      A.object
        [ "type" .= T.pack "KRoundedRectangleImpl",
          "children" .= (Seq.empty :: Seq.Seq A.Object),
          "actions" .= (Seq.empty :: Seq.Seq A.Object),
          "styles" .= styles,
          "cornerWidth" .= w,
          "cornerHeight" .= h,
          "properties"
            .= A.object
              [ "klighd.lsp.rendering.id" .= T.pack "$R0"
              ]
        ]
    KSpline styles -> simple "KSplineImpl" styles
    KRectangle styles -> simple "KRectangleImpl" styles
    KText text styles ->
      A.object
        [ "type" .= (T.pack "KTextImpl"),
          "actions" .= (Seq.empty :: Seq.Seq A.Object),
          "styles" .= styles,
          "properties"
            .= A.object
              [ "klighd.lsp.rendering.id" .= T.pack "$R0"
              ],
          "text" .= text
        ]
    KPolygon styles p ->
      A.object
        [ "type" .= T.pack "KPolygonImpl",
          "children" .= (Seq.empty :: Seq.Seq A.Object),
          "actions" .= (Seq.empty :: Seq.Seq A.Object),
          "styles" .= styles,
          "points" .= (Seq.fromList (buildCoords p)),
          "properties"
            .= A.object
              [ "klighd.lsp.rendering.id" .= T.pack "$R0"
              ]
        ]
    where
      simple r s =
        A.object
          [ "type" .= (T.pack r),
            "children" .= (Seq.empty :: Seq.Seq A.Object),
            "actions" .= (Seq.empty :: Seq.Seq A.Object),
            "styles" .= s,
            "properties"
              .= A.object
                [ "klighd.lsp.rendering.id" .= T.pack "$R0"
                ]
          ]

instance A.ToJSON GraphElement where
  toJSON element = case element of
    KNode
      { children = c,
        renderings = r,
        properties = p,
        gid = i
      } -> simple "node" c r p i
    KLabel {label = l, gid = i} ->
      A.object
        [ "type" .= T.pack "label",
          "text" .= l,
          "id" .= i,
          "properties"
            .= A.object
              [],
          "data"
            .= Seq.fromList
              [ A.object
                  [ "actions" .= (Seq.empty :: Seq.Seq A.Object),
                    "children" .= (Seq.empty :: Seq.Seq A.Object),
                    "properties"
                      .= A.object
                        [ "klighd.lsp.rendering.id" .= T.pack "$R0"
                        ],
                    "styles" .= (Seq.empty :: Seq.Seq A.Object),
                    "text" .= l,
                    "type" .= T.pack "KTextImpl"
                  ]
              ],
          "children" .= (Seq.empty :: Seq.Seq A.Object)
        ]
    KPort
      { children = c,
        renderings = r,
        properties = p,
        gid = i
      } -> simple "port" c r p i
    KEdge
      { children = c,
        renderings = r,
        properties = p,
        gid = i,
        source = s,
        target = t
      } ->
        A.object
          [ "data" .= Seq.fromList r,
            "type" .= T.pack "edge",
            "id" .= i,
            "properties" .= (M.fromList $ map toKV p),
            "children"
              .= Seq.fromList c,
            "sourceId" .= s,
            "targetId" .= t,
            "junctionPoints" .= A.object []
          ]
    KGraph
      { child = c,
        properties = p,
        gid = i
      } ->
        A.object
          [ "type" .= T.pack "graph",
            "revision" .= (0 :: Int),
            "id" .= i,
            "properties" .= (M.fromList $ map toKV p),
            "children"
              .= Seq.fromList [c]
          ]
    where
      simple t c r p i =
        A.object
          [ "data" .= Seq.fromList r,
            "type" .= T.pack t,
            "id" .= i,
            "properties" .= (M.fromList $ map toKV p),
            "children"
              .= Seq.fromList c
          ]
      toKV :: KProperty -> (T.Text, A.Value)
      toKV = \case
        NodeLabelsPlacement l -> ("org.eclipse.elk.nodeLabels.placement", A.toJSON $ Seq.fromList l)
        NodeSizeConstraints l -> ("org.eclipse.elk.nodeSize.constraints", A.toJSON $ Seq.fromList l)
        NodeSizeMinimum l -> ("org.eclipse.elk.nodeSize.minimum", A.toJSON $ Seq.fromList l)
        EdgeType v -> ("org.eclipse.elk.edge.type", A.toJSON v)
        JunctionPoints l -> ("org.eclipse.elk.junctionPoints", A.toJSON $ Seq.fromList l)
        LayerConstraint v -> ("org.eclipse.elk.layered.layering.layerConstraint", A.toJSON v)
        PortBorderOffset v -> ("org.eclipse.elk.port.borderOffset", A.toJSON v)
        PortSide v -> ("org.eclipse.elk.port.side", A.toJSON v)
        PortConstraints v -> ("org.eclipse.elk.portConstraints", A.toJSON v)
