{-# LANGUAGE FlexibleInstances #-}
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
  = NodeLabelsPlacement
  | NodeSizeConstraints
  | NodeSizeMinimum
  | EdgeType
  | JunctionPoints

instance Show KProperty where
  show NodeLabelsPlacement = "org.eclipse.elk.nodeLabels.placement"
  show NodeSizeConstraints = "org.eclipse.elk.nodeSize.constraints"
  show NodeSizeMinimum = "org.eclipse.elk.nodeSize.minimum"
  show EdgeType = "org.eclipse.elk.edge.type"
  show JunctionPoints = "org.eclipse.elk.junctionPoints"

-- We might want to change this later if we add the symbolic enum names
type KProperties = [(KProperty, [Int])]

-- | Styles which change how an object is rendered
data KStyle
  = KBackgroundColor Int Int Int
  | KForegroundColor Int Int Int

instance A.ToJSON KStyle where
  toJSON style = case style of
    KBackgroundColor r g b -> color "KBackgroundImpl" r g b
    KForegroundColor r g b -> color "KForegroundImpl" r g b
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
  | KRoundedBendsPolyline [KStyle] Int
  | KRectangle [KStyle]
  | KText T.Text [KStyle]

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
            "properties" .= (toProperties p),
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
            "properties" .= (toProperties p),
            "children"
              .= Seq.fromList [c]
          ]
    where
      simple t c r p i =
        A.object
          [ "data" .= Seq.fromList r,
            "type" .= T.pack t,
            "id" .= i,
            "properties" .= (toProperties p),
            "children"
              .= Seq.fromList c
          ]
      toProperties p = M.fromList $ map (\(a, b) -> (T.pack $ show a, Seq.fromList b)) p
