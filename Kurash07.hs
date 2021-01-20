{-# OPTIONS_GHC -Wall #-}
module Kurash07 where

import Data.Char(isSpace, isDigit, isLetter) 
import Data.Maybe(isJust, isNothing, fromJust)

type Name       = String
type Attributes = [(String, String)]
data XML        =  Text String | Element Name Attributes [XML] deriving (Eq, Show)

-- Задача 1 -----------------------------------------
spaces :: String -> String
spaces [] = []
spaces (x:xs) | isSpace x = spaces xs
              | otherwise = x:xs 
  
-- Задача 2 ----------------------------------------- 
manyT, value, manyN :: String -> (String,String)

-- sN
manyT [] = ([],[])
manyT (x:xs) | elem x ['<', '>'] = ([], (x:xs))
             | otherwise = (x:(fst next), (snd next))
 where next = manyT xs

-- sV
value [] = ([],[])
value (x:xs) | elem x ['"'] = ([], (x:xs))
             | otherwise = (x:(fst next), (snd next))
 where next = value xs

-- sT
manyN [] = ([],[])
manyN (x:xs) | (isLetter x || isDigit x || elem x ['.', '-']) = (x:(fst next), (snd next))
             | otherwise = ([], (x:xs))
 where next = manyN xs

-- Задача 3 -----------------------------------------
name, text, fullValue :: String ->  Maybe(String,String) 

name [] = Nothing
name (x:xs) | isLetter x = Just(x:fst(manyN xs), snd(manyN xs))
            | otherwise = Nothing

text [] = Nothing
text xs | null (fst(manyT xs)) = Nothing
        | otherwise = Just(fst(manyT xs), snd(manyT xs))

fullValue [] = Nothing
fullValue (x:xs) | x=='"' && not(null(snd(value xs))) && head(snd(value xs))=='"' 
 = Just(fst(value xs), tail(snd(value xs)))
                 | otherwise = Nothing
                 
-- Задача 4 -----------------------------------------
attrib :: String -> Maybe ((String,String),String) 
attrib [] = Nothing
attrib xs | isJust(name xs) && spaces(snd(namePart)) /= [] && equalSign == '=' &&
  isJust(fullValue afterEqualSign) = Just(( (fst namePart), attrValue), rest)
          | otherwise = Nothing
 where namePart = fromJust(name xs)
       equalSign = head(spaces(snd namePart))
       afterEqualSign = spaces(tail(spaces(snd namePart)))
       attrValue = fst(fromJust(fullValue afterEqualSign))
       rest = snd(fromJust(fullValue afterEqualSign))


manyAtt :: String -> Maybe (Attributes,String) 
manyAtt [] = Just([],[])
manyAtt xs | isJust(attrib xs) && isJust(attrib(spaces rest)) = Just(currAttr:(fst next), (snd next))
           | isJust(attrib xs) = Just([currAttr], rest)
           | otherwise = Nothing
  where currAttr = fst(fromJust(attrib xs))
        rest = snd(fromJust(attrib xs))
        next = fromJust(manyAtt(spaces rest))

-- Задача 5 -----------------------------------------
begTag :: String -> Maybe ((String,Attributes),String)
begTag [] = Nothing
begTag xs | null(tail xs) = Nothing
begTag (x:xs) | x=='<' && isJust(name xs) && isJust(manyAtt afterTagName) && 
                not(null afterAttrs) && (head afterAttrs) == '>'
                        = Just((tagName, fst attrs), tail afterAttrs)
              | x=='<' && isJust(name xs) && isNothing(manyAtt afterTagName) && 
                not(null afterTagName) && (head afterTagName) == '>'
                        = Just((tagName, []), tail(afterTagName))
              | otherwise = Nothing
  where tagName = fst(fromJust(name xs))
        afterTagName = spaces(snd(fromJust(name xs)))
        attrs = fromJust(manyAtt afterTagName)
        afterAttrs = spaces(snd attrs)

endTag :: String -> Maybe (String,String) 
endTag [] = Nothing
endTag xs | (length xs) < 4 = Nothing
endTag (x:xs) | x=='<' && head xs == '/' && isJust(tagName) && head(snd(fromJust tagName))=='>'
                     = Just(fst(fromJust tagName), tail(snd(fromJust tagName)))
              | otherwise = Nothing
  where tagName = name(tail xs)

-- Задача 6 -----------------------------------------
element :: String -> Maybe (XML,String) 
element [] = Nothing
element xs | isJust(begTag xs) && isJust(endTag afterBegTag) &&
             (fst beginningTagName) == (fst(fromJust(endTag(afterBegTag)))) 
             = Just(Element (fst beginningTagName) (snd beginningTagName) [], (snd(fromJust(endTag(afterBegTag)))))
           | isJust(begTag xs) && isJust(manyXML afterBegTag) && isJust(endTag afterManyXML) &&
             (fst beginningTagName) == (fst endingTag) 
             = Just(Element (fst beginningTagName) (snd beginningTagName) (fst manyXml), (snd endingTag))
           | otherwise = Nothing
  where beginingTag = fromJust(begTag xs)
        beginningTagName = fst(beginingTag)
        afterBegTag = snd(beginingTag)
        manyXml = fromJust(manyXML afterBegTag)
        afterManyXML = snd(manyXml)
        endingTag = fromJust(endTag afterManyXML)

xml :: String -> Maybe (XML,String)
xml [] = Nothing
xml xs | isJust(text xs) = Just(Text (fst textFromJust), (snd textFromJust))
       | isJust(element xs) = element xs
       | otherwise = Nothing
  where textFromJust = fromJust(text xs)

manyXML :: String -> Maybe ([XML],String)
manyXML [] = Just([],[])
manyXML xs | isJust(xml xs) && isJust(xml(snd firstXML)) 
             = Just((fst firstXML):(fst next), (snd next))
           | isJust(xml xs) && isNothing(xml(snd firstXML)) 
             = Just([(fst firstXML)], (snd firstXML))
           | otherwise = Nothing
  where firstXML = (fromJust(xml xs))
        next = fromJust(manyXML (snd firstXML))

-- Задача 7 -----------------------------------------
fullXML :: String -> Maybe XML 
fullXML [] = Nothing
fullXML xs | null(spaces xs) = Nothing
           | isJust(element(spaces xs)) && null(spaces(snd xmlElement)) 
             = Just (fst xmlElement)
           | otherwise = Nothing
  where xmlElement = fromJust(element(spaces xs))


stst1, stst2, stst3 :: String
stst1 = "<a>A</a>"
stst2 = "<a x=\"1\"><b>A</b><b>B</b></a>"
stst3 = "<a>\
      \<b>\
        \<c att=\"att1\">text1</c>\
        \<c att=\"att2\">text2</c>\
      \</b>\
      \<b>\
        \<c att=\"att3\">text3</c>\
        \<d>text4</d>\
      \</b>\
    \</a>" 


x1, x2, x3 :: XML
x1 = Element "a" [] [Text "A"]
x2 = Element "a"
            [("x","1")]
            [Element "b" [] [Text "A"],
             Element "b" [] [Text "B"]]
x3 = Element "a" 
            [] 
            [Element "b" 
                     [] 
                     [Element "c"
                              [("att","att1")] 
                              [Text "text1"],
                      Element "c" 
                              [("att","att2")]
                              [Text "text2"]],
             Element "b" 
                     [] 
                     [Element "c" 
                              [("att","att3")] 
                              [Text "text3"],
                      Element "d" 
                              [] 
                              [Text "text4"]]]

casablanca :: String 
casablanca
  = "<film title=\"Casablanca\">\n  <director>Michael Curtiz</director>\n  <year>1942\
    \</year>\n</film>\n\n\n"

casablancaParsed :: XML 
casablancaParsed
  = Element "film" 
            [("title","Casablanca")] 
            [Text "\n  ",
             Element "director" [] [Text "Michael Curtiz"],
             Text "\n  ",
             Element "year" [] [Text "1942"],
             Text "\n"]



