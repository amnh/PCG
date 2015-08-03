{-- |
Module      :  Functions for manipulating phylogeentic components
Description :  Takes data from parser functins and recodess into usable state 
Copyright   :  (c) 2014 Ward C. Wheeler, Division of Invertebrate Zoology, AMNH. All rights reserved.
License     :  

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met: 

1. Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer. 
2. Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution. 

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

The views and conclusions contained in the software and documentation are those
of the authors and should not be interpreted as representing official policies, 
either expressed or implied, of the FreeBSD Project.

Maintainer  :  Ward Wheeler <wheeler@amnh.org>
Stability   :  unstable
Portability :  portable (I hope)

-}

module Component
( baseDataToLeafNodes 
, PhyloNode(..)
, PhyloComponent
, PhyloForest
, NodeCode(..)
, getForestCostList
, getRootCosts
) where

import Data.List
import Data.Int
import qualified Data.Set as Set
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import Data.Maybe
import Debug.Trace
import ReadGraphs
import ReadFiles
import CharacterData
import qualified Parsimony as Pars
import Control.Parallel.Strategies
import Control.DeepSeq

-- | stuff for maxFloat
maxFloat = 1.0e32 --0x7f7ffff is max 32 bit FLOAT IEEE ~3.4e34

--type for nodes with phylodata--linked to DataMatrix
--need to add funcionts to modify these potentially
--storing costs may be a pain--get them by folding over components (adding root
--costs) maybe not so bad
--since only need "best" when searching

--cost of PhyloComponent based on Vector/List of BinaryComponent Char Cost pairs
--char cost linked to the binary tree (traversal, and rooting) that generated it
--need to save that info when returning overall best cost to PhyloComponent

--order nodes for both PhyloComponents and BinaryComponents (for each char) so
--can be retireved easily (perhaps last in Vector/List or create a tuple)

--List.sort is used for setting parents and children to make sure maintain
--left/right invariance

data PhyloNode = PhyloNode  { code :: NodeCode                --links to DataMatrix for terminal
                            , nodeName :: String            --fromNameList HTUcode for non-leaf, or Newick
                            , isTerminal :: Bool
                            , isRoot :: Bool
                            , isTreeNode :: Bool
                            , children :: [NodeCode]
                            , parents :: [NodeCode]
                            , preliminaryStates :: !CharacterSetList
                            , localCost :: !(V.Vector Float)
                            , totalCost :: !(V.Vector Float)
                            } deriving (Show)

type PhyloComponent = (V.Vector PhyloNode)
type PhyloForest = [PhyloComponent]
type NodeCode = Int

-- | functions to modify PhyloNode
modifyNodeName :: PhyloNode -> String -> PhyloNode
modifyNodeName pNode newName = pNode { nodeName = newName}

modifyParentListEtc :: PhyloNode -> [NodeCode] -> PhyloNode
modifyParentListEtc pNode newSet = 
    pNode { 
          parents = sort newSet
        , isTreeNode = True
    }

modifyParentList :: PhyloNode -> [NodeCode] -> PhyloNode
modifyParentList pNode newSet = 
    pNode { 
          parents = sort newSet
    }

modifyParentListTreeStatus :: PhyloNode -> [NodeCode] -> PhyloNode
modifyParentListTreeStatus pNode newSet = 
    pNode { 
          parents = sort newSet
        , isTreeNode = length newSet == 1 
    }

modifyParentAndChild :: PhyloNode -> [NodeCode] -> [NodeCode] -> PhyloNode
modifyParentAndChild pNode newChildren newParents =
    pNode {
          children = sort newChildren
        , parents = sort newParents
    }

modifyChildList :: PhyloNode -> [NodeCode] -> PhyloNode
modifyChildList pNode newSet = pNode { children = sort newSet}

modifyPreliminaryStates :: PhyloNode -> CharacterSetList -> PhyloNode
modifyPreliminaryStates pNode newSet = pNode { preliminaryStates = newSet}

modifyLocalCost :: PhyloNode -> V.Vector Float -> PhyloNode
modifyLocalCost pNode newCost = pNode { localCost = newCost}

modifyTotalCost :: PhyloNode -> V.Vector Float -> PhyloNode
modifyTotalCost pNode newCost = pNode { totalCost = newCost}

modifyPrelimLocalTotal :: PhyloNode -> CharacterSetList ->  V.Vector Float ->  V.Vector Float -> PhyloNode
modifyPrelimLocalTotal pNode cSL lC tC =
    pNode {
          preliminaryStates = cSL
        , localCost = lC
        , totalCost = tC
    }

modifyNamePrelimLocalTotal :: PhyloNode -> String -> CharacterSetList ->  V.Vector Float ->  V.Vector Float -> PhyloNode
modifyNamePrelimLocalTotal pNode newName cSL lC tC =
    pNode {
          nodeName = newName
        , preliminaryStates = cSL
        , localCost = lC
        , totalCost = tC
    }

-- | pullNames take list of GenPhyNetNodes and creates list of first element in
pullNames :: [GenPhyNetNode] -> [String]
pullNames x =
    if null x then []
    else 
        let (a, _, _) = head x
        in
        a : pullNames (tail x)

-- | getNodeNames gets all names from GenForest
getNodeNames :: [GenForest] -> [String]
getNodeNames x =
    if null x then []
    else 
        let y = concat (head x)
        in
        pullNames y ++ getNodeNames (tail x)
            
-- | baseDataToLeafNodes converts base Data array set to node structures for leaf
--taxa, vector of nodes (ForestPhyloNodes) for O(1) random accessa
--takes input list of Forests and return list of PhyloForest
baseDataToLeafNodes :: [GenForest] -> [PhyloForest] 
baseDataToLeafNodes inGraphs = 
    if null inGraphs then []
    else 
        makePhyloForest (head inGraphs) : baseDataToLeafNodes (tail inGraphs)      

-- | makePhyloForest makes an individual PhyloForest from a list
--of input Graphs
makePhyloForest :: GenForest -> PhyloForest
makePhyloForest inGraph =
    if null inGraph then []
    else 
        makePhyloComponent (head inGraph) : makePhyloForest (tail inGraph)          


-- | getNamesFromGenPhyNet extracts pair of lists of strings, terminals first then HTUs
getNamesFromGenPhyNet :: GenPhyNet  -> [String]
getNamesFromGenPhyNet inNet =
    if null inNet then []
    else 
        let (firstName, desc,  anc) = head inNet
        in 
            if null desc then firstName : getNamesFromGenPhyNet (tail inNet)
            else getNamesFromGenPhyNet (tail inNet) ++ [firstName]
            
-- | getCodeNodePair cretes alist of pairs of indexCodes and PhyloNodes for
--reordering in the Vector to allow for effiecenit traversal access
getCodeNodePair :: PhyloComponent -> [(Int, PhyloNode)]
getCodeNodePair phyCom =
    if V.null phyCom then []
    else 
        let curNode = V.head phyCom 
        in
        (code curNode, curNode) : getCodeNodePair (V.tail phyCom)


-- | makePhyloComponent take a GenPhyNet (input component) and makes list of names
--in component and passes to Rec version to make phylocomponent
--need to reorder based on codes here so traversal work within component
makePhyloComponent ::  GenPhyNet -> PhyloComponent
makePhyloComponent inNet =
    if null inNet then error "No nodes in GenPhyNet"
    else 
        let nameList = getNamesFromGenPhyNet inNet
            initPhyloComponent = makePhyloComponentRec nameList inNet 0
            newOrder = getCodeNodePair initPhyloComponent
        in
        --trace ("\ninitial phylocomponent " ++ show inNet ++ " to " ++ show (initPhyloComponent V.// newOrder))
        initPhyloComponent V.// newOrder

-- | makePhyloComponentRec take a GenPhyNet (input component) and make Vector
--PhyloComp
makePhyloComponentRec :: [String] -> GenPhyNet -> Int -> PhyloComponent  
makePhyloComponentRec nameList inNet indexCode =
    if null inNet then V.empty
    else 
        V.cons (makePhyloNode nameList (head inNet) indexCode) 
            (makePhyloComponentRec nameList (tail inNet) (indexCode + 1))

-- | getCodes takes name list and return list of element numbers for code
--assignments in PhyloNode
getCodes :: [String] -> [String] -> [NodeCode]
getCodes allNames inNames =
    if null inNames then []
    else 
        --trace ("\ngetCodes " ++ show (head inNames) ++ " " ++ show allNames)
        fromJust  (elemIndex (head inNames) allNames) : getCodes allNames (tail inNames)



-- | makePhyloNode takes an individual GenPOhyNetNode and converts into PhyloNode
makePhyloNode :: [String] -> GenPhyNetNode -> Int -> PhyloNode
makePhyloNode nameList inNode indexCode =
        let (inName, descList, ancList) = inNode
            isLeaf = null descList
            isRootNode = null ancList
            codeAncList = getCodes nameList ancList
            codeDescList = getCodes nameList descList
            isTree = (length ancList < 2) --root and regular node
            numericalCode = elemIndex inName nameList
        in
        PhyloNode {
          code = fromJust numericalCode --later synched up with Vector index 
        , nodeName = inName
        , isTerminal = isLeaf
        , isRoot = isRootNode
        , isTreeNode = isTree
        , children = sort codeDescList
        , parents = sort codeAncList
        , preliminaryStates = [] 
        , localCost = V.singleton 0
        , totalCost = V.singleton 0
        }

-- | splitDataByComponent take DataMatrix and phyloComponent and returns data from
--component name list--should split would be more efficeint
splitDataByComponent :: DataMatrixVLS -> [String] -> PhyloComponent -> DataMatrixVLS
splitDataByComponent inData termNameList inComponent =
    if V.null inComponent then V.empty  
    else 
        let firstName = V.head inComponent
            inCompName = nodeName firstName
            nameIndex = elemIndex inCompName termNameList 
        in
        if isNothing nameIndex then splitDataByComponent inData termNameList (V.tail inComponent)
        else V.cons (inData V.! fromJust nameIndex)  (splitDataByComponent inData termNameList (V.tail inComponent))

-- | getForestCostList takes data matrix, list of input PhyloForest, and charinfo  and returns list of costs
getForestCostList :: DataMatrixVLS -> [PhyloForest] -> [CharInfo] -> [String] -> [Float]
getForestCostList dataMatrix  inForList charInfoList termNameList =
    getForestCost dataMatrix  (head inForList) charInfoList termNameList : 
        getForestCostList dataMatrix (tail inForList) charInfoList termNameList

-- | getForestCost returns cost of single forest
getForestCost :: DataMatrixVLS -> PhyloForest -> [CharInfo] -> [String] -> Float
getForestCost dataMatrix inFor charInfoList termNameList = 
    if null inFor then 0
    else 
        let compData = splitDataByComponent dataMatrix  termNameList (head inFor)
        in
        getComponentCost compData (head inFor) charInfoList + 
            getForestCost dataMatrix (tail inFor) charInfoList termNameList

-- | phyloComponentToTreeList takes PhyloComponent and returns list of trees
--"displayed" for subsequent traversal and diagnosis
--need to "split" every time traversal hits a network node making a Tree 
--by makeing one of the parent nodes indegree 1 outdegree 1
--errors if resolution yeilds a terminal intenal node
--Basically
--  Examine nodes (Vector) in turn
--      if nodes is tree node, add to Vector
--      else if netowrk node
--          "split" Vector (use V.\\ to modify nodes and make new Vector)
--              in first 
--                  take head of parent list
--                  modify parent list of that node
--                  modify descendent list of non-head parents
--                  recurse to next node
--              in second
--                  duplicate Vector deleting 2nd parent 
--                  delete descendet of 2nd parent
--                  recurse to next node
--          
phyloComponentToTreeList :: PhyloComponent -> [PhyloComponent]
phyloComponentToTreeList inPhyloComp =
    if V.null inPhyloComp then error "No phylo component to resolve"
    else 
        --search for network nodes
        let retTreeList = binarizeComponent [inPhyloComp] 0  (V.length inPhyloComp)
        in
        --trace("Binarized list: " ++ show retTreeList ++ "\n\n")
        retTreeList

-- | binarizeComponent recursivelt splits PhyloCOmpoennt at first network node, adds to list of trees,
--and does this through growing list for all splits
binarizeComponent :: [PhyloComponent] -> Int -> Int -> [PhyloComponent]
binarizeComponent inCompList index maxIndex
  | null inCompList = []
  | index == maxIndex = inCompList
  | otherwise =
    let curComp = head inCompList
        splitComp
          = splitAndModifyComponent (curComp V.! index) index curComp ++
              binarizeComponent (tail inCompList) index maxIndex
    in binarizeComponent splitComp (index + 1) maxIndex

-- | clearNonParents takes index and list of parents to clear 
clearNonParents :: Int -> [Int] -> PhyloComponent -> [(Int, PhyloNode)]
clearNonParents child parentsToClear phyloComponent = 
    if null parentsToClear then []
    else 
        let parent = head parentsToClear 
            origChildList =  children (phyloComponent V.! parent) 
            newChildList = filter ( /= child) origChildList
            newParent = modifyChildList (phyloComponent V.! parent) newChildList
        in
        --trace ("\nClearing parent " ++ show parent ++ "org child list " ++ show origChildList ++ " new " ++ 
        --    show newChildList ++ " to make " ++ show newParent)
        if null newChildList then [] --this if HTU with no children--impossible binary resolution of network
        else (parent, newParent) : clearNonParents child (tail parentsToClear) phyloComponent

-- | generateBinaryResolutions takes a network node with indegree = n (can be
--greater that 2) and the position of the node in Vector of nodes and return
--list of resolutions at that position
generateBinaryResolutions :: PhyloNode -> Int -> PhyloComponent -> [Int] -> [Int] -> [PhyloComponent]
generateBinaryResolutions inNode index totalComponent parentList fullParentList =
    if null parentList then []
    else 
        let curParent = head parentList
            otherParents = filter ( /= curParent) fullParentList
            newThisNode = modifyParentListEtc inNode [curParent]
            modList = (index, newThisNode) : clearNonParents index otherParents totalComponent
        in
        if length modList > 1 then  --this is check for impossible resolutions of network nodes
            (totalComponent V.// modList) : generateBinaryResolutions inNode index totalComponent (tail parentList) fullParentList
        else 
            --error ("Error in display tree creation: Resolution of node yields internal node as terminal" ++ show modList)
            generateBinaryResolutions inNode index totalComponent (tail parentList) fullParentList


-- | splitAndModifyComponent modifies phylocomponent returning two PhyloComponents, should work for indegree > 2
splitAndModifyComponent :: PhyloNode -> Int -> PhyloComponent -> [PhyloComponent]
splitAndModifyComponent inNode index totalComponent
    | isTreeNode inNode = [totalComponent]
    | length (parents inNode) < 2 =
      error ("This can't happen (too few parents)" ++ show inNode)
    | otherwise =
      let parentList = parents inNode in
        generateBinaryResolutions inNode index totalComponent parentList
          parentList

-- | getBinaryCostList takes list of binary trees and returns list of costs
--this needs to be list of costs per character to be minimized over characters
getBinaryCostList :: [CharInfo] -> DataMatrixVLS -> PhyloComponent -> V.Vector PhyloComponent -> V.Vector (V.Vector Float)
getBinaryCostList charInfoList dataMatrix previousBinaryTree binTreeList =
    if V.null binTreeList then V.empty
    else 
        let curBinTree = V.head binTreeList 
            startNode = V.last curBinTree  --assumes root last--change to getRootCode?
            !updatedPhyloComponent = traverseComponent dataMatrix curBinTree startNode charInfoList previousBinaryTree
            newOrder = getCodeNodePair updatedPhyloComponent
            !reorderedUpdatedPhyloComponent = updatedPhyloComponent V.// newOrder
        in
        trace ("TC:" ++ show (totalCost (V.last reorderedUpdatedPhyloComponent)) ++ " ")
        V.cons (totalCost (V.last reorderedUpdatedPhyloComponent))  --assumes root last getRootCode?
            (getBinaryCostList charInfoList dataMatrix reorderedUpdatedPhyloComponent  (V.tail binTreeList))

-- | compileBinaryCosts gets the costs of eachbinary tree
compileBinaryCosts :: V.Vector (V.Vector Float) -> V.Vector Float
compileBinaryCosts costListList =
    if V.null costListList then V.empty 
    else V.cons (V.sum (V.head costListList))  (compileBinaryCosts (V.tail costListList))

-- | compileSoftCost gets the costs of eachbinary tree
compileSoftCosts :: V.Vector (V.Vector Float) -> V.Vector Float
compileSoftCosts costListList =
    if V.null costListList then V.empty
    else 
        getPositionMin costListList 0

-- | minOfList get min cost of column of vectors
minOfList :: V.Vector (V.Vector Float) -> Int -> Float -> Float
minOfList costListList position curMin =
    if V.null costListList then curMin
    else 
        let curCost =  (V.head costListList) V.! position
        in
        if curCost < curMin then minOfList (V.tail costListList) position curCost
        else minOfList (V.tail costListList) position curMin

-- | getPositionMin takes vector of vector of costs and returns minimum of costs
--over charcaters for soft-wired cost
getPositionMin :: V.Vector (V.Vector Float) -> Int -> V.Vector Float
getPositionMin costList position =
    if position == V.length (V.head costList) then V.empty
    else 
        let getPositionCost = minOfList costList position maxFloat
        in
        V.cons getPositionCost  (getPositionMin costList (position + 1))

-- | getReticulateCount take a PhyloComponent and returns number of reticulate
--edges, num parents - 1 for all (non root)
getReticulateEdges :: Int -> PhyloComponent -> Int
getReticulateEdges prevNum inComp =
    if (V.null inComp) then prevNum
    else 
        let numParents = length (parents (V.head inComp))
        in
        getReticulateEdges (prevNum + numParents - 1) (V.tail inComp)

-- | getSoftAdjust this is an added cost of network edges r/2 * bestCost / (2n -2)
getSoftAdjust :: Int -> Float -> Int -> Float
getSoftAdjust numReticulateEdges softCost numTerminals =
    if numTerminals == 1 then 0
    else (fromIntegral numReticulateEdges) * softCost / fromIntegral (2 * ((2 * numTerminals)  - 2))

-- | getDisplayTreeCostList spits cost list into display trees (resolutions), 
--with lists of best rooted cost for each character
getDisplayTreeCostList :: [V.Vector PhyloComponent] -> V.Vector (V.Vector Float) -> V.Vector (V.Vector Float)
getDisplayTreeCostList rerootedList charCostVectVect = 
   if V.null charCostVectVect then V.empty
   else 
       let  rootsPerTree = V.length (head rerootedList)  
            displayRoots = V.take rootsPerTree charCostVectVect
            remainderDisplayRoots = V.drop rootsPerTree charCostVectVect
            displayCharCostVect = compileSoftCosts displayRoots
       in
       V.cons displayCharCostVect  (getDisplayTreeCostList (tail rerootedList) remainderDisplayRoots)

-- | getBinCosts take list of char costs by tree and returns list of sums
getBinCosts :: V.Vector (V.Vector Float) -> V.Vector Float
getBinCosts displayCharCosts =
    if V.null displayCharCosts then V.empty
    else 
        let bTree = V.sum $ V.head displayCharCosts
        in
        V.cons bTree (getBinCosts $ V.tail displayCharCosts)

-- | getCharDisplayIndices takes best cost for each charcaet and returns list of
--indices of the display tree that cost was found on 
getCharDisplayIndices :: V.Vector Float -> V.Vector (V.Vector Float) -> V.Vector (V.Vector Int)
getCharDisplayIndices softCostList displayTreeCharCostList =
    if V.null softCostList then V.empty
    else 
        getPositionElement softCostList displayTreeCharCostList 0 

-- | getMatchTree  takes a value, position and vector of vetcor fo floats and
--returns element match index Vector
getMatchTree :: V.Vector Float -> V.Vector (V.Vector Float) -> Int -> Int -> V.Vector Int
getMatchTree  softCostList displayTreeCharCostList position dTree =
    if V.null displayTreeCharCostList then V.empty
    else 
        if softCostList V.! position == (V.head displayTreeCharCostList) V.! position then
            V.cons dTree (getMatchTree  softCostList (V.tail displayTreeCharCostList) position (dTree + 1)) 
        else (getMatchTree  softCostList (V.tail displayTreeCharCostList) position (dTree + 1))

-- | getPositionElement check each element at a position to see if it equals minium
--value and retuns list of elements
getPositionElement ::  V.Vector Float -> V.Vector (V.Vector Float) -> Int -> V.Vector (V.Vector Int)
getPositionElement softCostList displayTreeCharCostList position =
    if position == (V.length softCostList) then V.empty
    else
        let matchTree = getMatchTree softCostList displayTreeCharCostList position 0
        in        
        V.cons matchTree (getPositionElement softCostList displayTreeCharCostList (position + 1)) 

-- | getSoftAdjust2 this is an added cost of network edges r/2 * bestCost / (2n -2)
getSoftAdjust2 :: Int -> V.Vector Float -> Int -> V.Vector (V.Vector Int) -> Float 
getSoftAdjust2 bestTreeIndex bestTreeCharCostList numTerminals bestCharIndicesList =
    if numTerminals == 1 then 0
    else if V.null bestTreeCharCostList then 0
    else
        let charCost = V.head bestTreeCharCostList 
            numEdges =  fromIntegral (2 * ((2 * numTerminals)  - 2)) --actually 2 x num edges for reduction of 1/2 in expectation
            charPenalty = charCost / numEdges
            bestDisplayTreeCharList = V.head bestCharIndicesList
            --firstDisplayTreeChar = V.head bestDisplayTreeCharList --arbitrarily take first one if multiple 
        in
        --if (firstDisplayTreeChar /= bestTreeIndex) then
        if (V.notElem bestTreeIndex bestDisplayTreeCharList) then
            trace (" P2 " ++ show charPenalty) charPenalty  +  
                (getSoftAdjust2 bestTreeIndex (V.tail bestTreeCharCostList) numTerminals (V.tail bestCharIndicesList))
        else
            (getSoftAdjust2 bestTreeIndex (V.tail bestTreeCharCostList) numTerminals (V.tail bestCharIndicesList))

-- | getSoftAdjust3 this is an added cost of network edges r/2 * bestCost / (2n -2)
-- but multiplied by number of edges not in best binary, so >= SoftAdjust2
-- edges include indegree=outdegree=1 in number different. num edges for base
-- cost is as if htere were no indegree=outdegree=1 nodes.  Could adjust for
-- this--but really may make no difference unless there are superfluous nodes in
-- network.  
getSoftAdjust3 :: Int -> V.Vector Float -> Int -> V.Vector (V.Vector Int) -> [PhyloComponent] -> Set.Set (Int, Int) -> Float 
getSoftAdjust3 bestTreeIndex bestTreeCharCostList numTerminals bestCharIndicesList displayTreeList bestDisplayEdgeSet  =
    if numTerminals == 1 then 0
    else if V.null bestTreeCharCostList then 0
    else
        let bestDisplayEdgeSet = edgeSetFromComponent (displayTreeList !! bestTreeIndex)
            bestDisplayTreeCharList = V.head bestCharIndicesList  
            bestCharDisplayEdgeSet = edgeSetFromComponent (displayTreeList !! (V.head bestDisplayTreeCharList)) --arbitrarily take first one if multiple 
            edgeDiffNum = Set.size $ Set.difference bestCharDisplayEdgeSet bestDisplayEdgeSet --edges in Char best tree not in overall best binary tree
            charCost = V.head bestTreeCharCostList 
            numEdges =  fromIntegral (2 * ((2 * numTerminals)  - 2)) --actually 2 x num edges for reduction of 1/2 in expectation
            charPenalty = (fromIntegral edgeDiffNum) * charCost / numEdges
        in
        if (V.notElem bestTreeIndex bestDisplayTreeCharList) then
            trace (" P2 " ++ show charPenalty) charPenalty  +  
                (getSoftAdjust3 bestTreeIndex (V.tail bestTreeCharCostList) numTerminals (V.tail bestCharIndicesList) displayTreeList bestDisplayEdgeSet)
        else
            (getSoftAdjust3 bestTreeIndex (V.tail bestTreeCharCostList) numTerminals (V.tail bestCharIndicesList) displayTreeList bestDisplayEdgeSet)

-- | edgeCodeToName takes Edge codes numbers and returns Node Name
edgeCodeToName :: Int -> PhyloComponent -> String
edgeCodeToName nodeCode inNodes =
    if nodeCode < 0 || (nodeCode - 1 ) > V.length inNodes then 
        error ("Impossible node code " ++ show nodeCode ++ " component size " ++ show (V.length inNodes))
    else 
        nodeName (inNodes V.! nodeCode)

-- | edgePairListStringPairList takes list of edges and returns String Pairs for
-- each edge 
edgePairListStringPairList :: [(Int, Int)] -> PhyloComponent -> [(String, String)]
edgePairListStringPairList edgeList inNodes =
    if null edgeList then []
    else 
        let (a, b) = head edgeList
            c = edgeCodeToName a inNodes
            d = edgeCodeToName b inNodes
        in
        (c, d) : edgePairListStringPairList (tail edgeList) inNodes


-- | getComponentCost returns cost of component from an input node (sum of all char
--total costs)--checks if root
--need to add root cost (for forest optimization) and MaxFloat for unused 
--edges so exclude Forests/Coponents with superfluous edges.
getComponentCost :: DataMatrixVLS -> PhyloComponent -> [CharInfo] -> Float
getComponentCost dataMatrix inComp charInfoList =
    if V.null inComp then 0
    else 
        let startNode = V.last inComp
        in
        if not (isRoot startNode) then error "Start element of phylocomponent is not component root"
        else --split here for list of binary components--naive at first--complete components
             --lists of phylocompoents should be changed to vectors for better
             --access when doing incremental optimizations--lots to reuse 
            let displayTreeList = phyloComponentToTreeList inComp
                inCompEdgeSet = edgeSetFromComponent inComp
                reRootedVectList = getReRootList displayTreeList --change to list of Vetors etc to keep trac of rerootlengths
                {-
                reRootedVect = V.concat reRootedVectList--could do a parmap here
                charCostVectVect = getBinaryCostList charInfoList dataMatrix V.empty reRootedVect
                -}
                !charCostVectVect =  V.concat $ parMap rdeepseq (getBinaryCostList charInfoList dataMatrix V.empty) reRootedVectList
                displayTreeCharCostList = getDisplayTreeCostList reRootedVectList charCostVectVect --error here I think number reoots may vary?
                displayTreeCostList = getBinCosts displayTreeCharCostList
                allCosts = compileBinaryCosts charCostVectVect --really for debug purposes
                softCostList = compileSoftCosts displayTreeCharCostList --charCostVectVect
                softCost = V.sum softCostList
                bestDisplayIndices = V.elemIndices (V.minimum displayTreeCostList) displayTreeCostList
                charDisplayIndices = getCharDisplayIndices softCostList displayTreeCharCostList
                --inCompEdgeSet = edgeSetFromComponentList displayTreeList
                unusedEdges = Set.toList $ Set.difference inCompEdgeSet 
                    (edgeSetFromComponentListSome displayTreeList 
                    (nub $ listOfVector $ V.toList charDisplayIndices) 0)
                numReticulateEdges = getReticulateEdges 0 (V.init inComp) 
                softAdjust = getSoftAdjust numReticulateEdges softCost (V.length dataMatrix)
                --arbitrarily uses first `best` binary tree
                softAdjust2 = getSoftAdjust2 (V.head bestDisplayIndices) 
                    (displayTreeCharCostList V.! (V.head bestDisplayIndices))  
                    (V.length dataMatrix) charDisplayIndices 
                softAdjust3 = getSoftAdjust3 (V.head bestDisplayIndices) 
                    (displayTreeCharCostList V.! (V.head bestDisplayIndices))  
                    (V.length dataMatrix) charDisplayIndices displayTreeList (edgeSetFromComponent $ displayTreeList !! (V.head bestDisplayIndices))
                rootCost = getRootCosts charInfoList --make into a fold   
             in 
                trace ("\nBinaries : " ++ show (length displayTreeList) ++ " " ++ show (V.length $ V.concat reRootedVectList) ++ " " 
                    ++ show (V.length charCostVectVect) ++ " " ++ show allCosts ++ " "
                    ++ show softCostList ++ "\nDisplay Costs " ++ show displayTreeCostList ++ " best tree " ++ show bestDisplayIndices 
                    ++ " -> " ++ show (V.minimum displayTreeCostList) ++ "\nsoft " ++ show softCost ++ " soft adjust " ++ show softAdjust 
                    ++ "\nSoft Indices " ++ show charDisplayIndices ++ "\nSoft-2 "  ++ show softAdjust2 ++ " -> " ++ show (softCost + softAdjust2)
                    ++ " Soft-3 "  ++ show softAdjust3 ++ " -> " ++ show (softCost + softAdjust3)
                    ++ "\nDisplay Trees " ++ show (binaryToNewick displayTreeList) 
                    ++ "\nUnused Edges " ++ show (edgePairListStringPairList unusedEdges inComp)
                    -- ++ "\nDisplay edges " ++ show (edgePairListStringPairList (Set.toList inCompEdgeSet) inComp)
                    -- ++ "\nUsed edges " ++ show (edgePairListStringPairList (Set.toList $ edgeSetFromComponentListSome displayTreeList (nub $ listOfVector $ V.toList charDisplayIndices) 0) inComp)
                    -- ++ "\nRoot cost: " ++ show rootCost
                    -- ++ "\nFrom : " ++ show inComp
                    ) 
                --"\nInput " 
                --    ++ show inComp ++ "\nBin " ++ show displayTreeList)( 
                (
                if null unusedEdges then softCost + softAdjust2 + rootCost -- Need to add root cost here sum over charInfo rootCosts.
                else maxFloat --V.minimum charCostVectVect
                )

-- | getRootCosts sums over root costs in CharInfo
-- really should be 1/2 subcost for length of character,
-- so needs to be adjusted for alternate costs matricces (sankoff, DO)
getRootCosts :: [CharInfo] -> Float
getRootCosts charInfoList =
    if null charInfoList then 0.0
    else 
        let firstChar = head charInfoList
        in
        if activity firstChar then ((weight firstChar) * (rootCost firstChar)) + (getRootCosts $ tail charInfoList)
        else (getRootCosts $ tail charInfoList)


-- | getReRootList takes list of binary trees and returns list of all reroots of
--all binary trees
getReRootList :: [PhyloComponent] -> [V.Vector PhyloComponent]
getReRootList inBinaryList =
    if null inBinaryList then []
    else 
        --trace ("num reroots " ++ show (V.length $ getReRoots (head inBinaryList) )) (
        (getReRoots (head inBinaryList)) :  (getReRootList (tail inBinaryList))
        --)

-- | getReRoots inputs a single binary phylocomponent and returns a list of all
--reroots
getReRoots :: PhyloComponent -> V.Vector PhyloComponent
getReRoots inBinaryTree =
    if V.null inBinaryTree then V.empty
    else 
        let rootNode = (V.last inBinaryTree)
        in
        if not (isRoot rootNode) then error "Last node not root in reroot binary"
        else 
            let leftChild = head (children rootNode)
                rightChild = last (children rootNode) --assumes binary
                deRootedTree = deRootComp inBinaryTree leftChild rightChild
                reRootList = rootOnEdges (V.init deRootedTree) deRootedTree leftChild rightChild (code rootNode)
            in
            V.cons inBinaryTree  reRootList --include original for its root

-- | deRootComp deroots tree based on root node children
--assigns the parent of left desc of root to right desc and visa versa
--this so when rerooted, the node changes can stop at these nodes
--This might be unnecessary, depending on stopping rule for updating nodes in
--rootOn Edges
deRootComp :: PhyloComponent -> Int -> Int  -> PhyloComponent
deRootComp inBinaryTree leftChild rightChild =
        let newLeftNode = modifyParentList (inBinaryTree V.! leftChild) [rightChild]
            newRightNode = modifyParentList (inBinaryTree V.! rightChild) [leftChild]
        in
        inBinaryTree V.// [(leftChild, newLeftNode), (rightChild, newRightNode)] 

-- | rootOnEdge takes a derooted binary tree, its original root children (root should be last in 
--vector list) and reroots tree on the edge leading to each node in turn, but
--not the original (which is added back to list in getReRoots).  The rootNode is
--modified as are all nodes that need a parent and child switched
--inUnrootedComponent contains teh nodes to be rerooted and is depleted asn teh
--operation proceeds, origUnRooted allows for new trees to be constructed
--"left" and "right" refer to the nodes positions wrt the new root. "Left" is
--the descendent node and "right" the parent node of the rerooted edge.
rootOnEdges :: PhyloComponent -> PhyloComponent -> Int -> Int -> Int -> V.Vector PhyloComponent
rootOnEdges inUnRootedComp origUnRootedComp origLeft origRight rootCode =
    if V.null inUnRootedComp then V.empty -- does not include root node
    else 
        let newRootLeft = V.head inUnRootedComp
            codeLeft = code newRootLeft
        in  --add codes so not redo original or indegree 1 outdegree 1 nodes
            if (codeLeft == origLeft) || (codeLeft == origRight) || ((length $ children newRootLeft) == 1) then
                    rootOnEdges (V.tail inUnRootedComp) origUnRootedComp origLeft origRight rootCode
            else 
                --chose edge to reroot by terninating node
                --get parent of terminating node to define edge
                let codeRight =  head $ parents newRootLeft --assumes binary
                    newRoot = modifyChildList (V.last origUnRootedComp) [codeLeft, codeRight] 
                    newModLeft = modifyParentList (origUnRootedComp V.! codeLeft) [rootCode]
                    --newRightChildren =  (parents (origUnRootedComp V.! codeRight)) ++ (filter (/= codeLeft) (children (origUnRootedComp V.! codeRight)))
                    --newModRight = modifyParentAndChild (origUnRootedComp V.! codeRight) newRightChildren [rootCode]
                in
                let modNodes = [(codeLeft, newModLeft), (rootCode, newRoot)] ++ --[(codeRight, newModRight)] ++
                        (rerootNextParent origUnRootedComp origLeft origRight codeLeft codeRight rootCode) --codeRight (head (parents (origUnRootedComp V.! codeRight))))
                    newComp = origUnRootedComp V.// modNodes
                in
                {-trace ("Rerooted on " ++ show codeLeft ++ " " ++ show codeRight ++ ":\n " ++ show origUnRootedComp ++
                    "\n-> " ++ show newComp)
                -}
                V.cons newComp (rootOnEdges (V.tail inUnRootedComp) origUnRootedComp origLeft origRight rootCode) 

-- | rerootNextParent goes down teh tree parent to parent updating each node by
--swapping parents and child until a node is unchanged or it hits the original
--egde/root position.
rerootNextParent :: PhyloComponent -> Int -> Int -> Int -> Int -> Int -> [(Int, PhyloNode)]
rerootNextParent origUnRootedComp origLeft origRight nodeFrom nodeToReroot rootCode =
    if nodeToReroot == origLeft then --special case and return, only need to modify left since parents were set to each other on deroot
        let newChildren = origRight : (filter (/= nodeFrom) (children (origUnRootedComp V.! nodeToReroot)))
            newNode = modifyParentAndChild (origUnRootedComp V.! nodeToReroot) newChildren [rootCode]
        in
        [(nodeToReroot, newNode)] 
    else if nodeToReroot == origRight then --special case and return--symmetrical to above
        let newChildren = origLeft : (filter (/= nodeFrom) (children (origUnRootedComp V.! nodeToReroot)))
            newNode = modifyParentAndChild (origUnRootedComp V.! nodeToReroot) newChildren [rootCode]
        in
        [(nodeToReroot, newNode)]  
    else --regular parent node to reroot, compare orig and new parent, if same return [], 
         --else modify node and recurse to its parent
        let thisParent = head (parents (origUnRootedComp V.! nodeToReroot))
        in
        if thisParent == nodeFrom then []
        else 
            let origParent = head (parents (origUnRootedComp V.! nodeToReroot))
                newChildren = origParent : (filter (/= nodeFrom) (children (origUnRootedComp V.! nodeToReroot)))
                newNode = modifyParentAndChild (origUnRootedComp V.! nodeToReroot) newChildren [nodeFrom]
            in 
            (nodeToReroot, newNode) : (rerootNextParent origUnRootedComp origLeft origRight nodeToReroot origParent rootCode)


-- | makePrelim takes CharacterSetList if preliminary states of left and right
--children to create the prelim states for cur node
--THIS IS A PLACEHOLDER
makePrelim :: CharacterSetList -> CharacterSetList -> [CharInfo] -> [(BaseChar, Float)]
makePrelim lStates rStates charInfoList =
    if null lStates || null rStates || null charInfoList then []
    else 
        if False then
            let lState = head lStates
                rState = head rStates
                charInfo = head charInfoList
                newStatesCost = Pars.getPrelim lState rState charInfo 
                --(VS.singleton (0 :: Int64), 0) --(state, cost) --placeholder to optimization
            in
            --trace ("Optimizing " ++ show (length lStates) ++ " characters")
            newStatesCost : makePrelim (tail lStates) (tail rStates) (tail charInfoList)
        else 
            parMap rdeepseq Pars.getPrelimTriple (zip3 lStates rStates charInfoList)

-- | traverseComponent takes, data, a node and current phylo vector and traverses netwrok 
--according to the phylonode input component 
----the function updates the phylo tree and preliminary dat etc as is proceeeds
--post-order
--Will need to be rerooted and root tracked (generating multiple preliminaries
--for dynamic charcaters)
traverseComponent ::  DataMatrixVLS -> PhyloComponent -> PhyloNode -> [CharInfo] -> PhyloComponent -> PhyloComponent
traverseComponent dataMatrix inComp curPNode charInfoList previousBinaryTree
  | not (isTreeNode curPNode) =
    error ("Should not hit network node in traversal" ++ show curPNode)
  | isTerminal curPNode =
    let allZero = V.replicate (length (V.head dataMatrix)) 0 in
      V.singleton
        (modifyPrelimLocalTotal curPNode (dataMatrix V.! code curPNode)
           allZero
           allZero)
  | (not $ isTerminal curPNode) && (length (children curPNode) == 0) = --promoted internal vertex
    let allZero = V.replicate (length (V.head dataMatrix)) 0 in
      V.singleton
        (modifyPrelimLocalTotal curPNode (dataMatrix V.! code curPNode)
           allZero
           allZero) 
  | length (children curPNode) > 2 =
    error "Descendant polytomies not yet implemented"
  | length (children curPNode) == 1 =
    let onlyNodeCode = head (children curPNode)
        onlyChild
          = traverseComponent dataMatrix inComp (inComp V.! onlyNodeCode) 
            charInfoList previousBinaryTree
        thisName
          = "(" ++
            nodeName curPNode ++ "=" ++ nodeName (V.head onlyChild) ++ ")"
        thisNode
          = modifyNamePrelimLocalTotal curPNode thisName
            (preliminaryStates (V.head onlyChild))
            (localCost (V.head onlyChild))
            (totalCost (V.head onlyChild))
    in (V.singleton thisNode) V.++ onlyChild
  | otherwise =
    --trace ("\nUpdated Component:" ++ show curPNode)
    --change this so check only namae first--if not then do the traversals left
    --and right--or-maybe the lazy does it.
        (let leftNodeCode = head (children curPNode)
             rightNodeCode = last (children curPNode)
             leftResult
               = traverseComponent dataMatrix inComp (inComp V.! leftNodeCode) 
                   charInfoList previousBinaryTree
             rightResult
               = traverseComponent dataMatrix inComp (inComp V.! rightNodeCode) 
                   charInfoList previousBinaryTree
             thisName --check here for already done in previous rootings/trees, should control for left/right name issues
               = "(" ++  
                   (min (nodeName (V.head leftResult))  (nodeName (V.head rightResult))) ++
                     "," ++ (max (nodeName (V.head leftResult))  (nodeName (V.head rightResult)))  ++ ")"
         in
            if thisName == (getPrevName previousBinaryTree (code curPNode)) then
                let thisNode = modifyNamePrelimLocalTotal curPNode thisName  (preliminaryStates previousTreeNode)
                        (localCost previousTreeNode) (totalCost previousTreeNode)
                    previousTreeNode = previousBinaryTree V.! (code curPNode)
                in
                (V.singleton thisNode) V.++ (leftResult V.++ rightResult) 
            else
                let prelimStatesCost
                        = makePrelim (preliminaryStates (V.head leftResult))
                        (preliminaryStates (V.head rightResult))
                        charInfoList
                    sumThreeCosts
                        = V.zipWith3 (\ a b c -> a + b + c) (totalCost (V.head leftResult))
                        (totalCost (V.head rightResult))
                        (extractNodeCosts prelimStatesCost) --thisNodeCosts
                    thisNode
                        = modifyNamePrelimLocalTotal curPNode thisName (extractNodeStates prelimStatesCost) 
                        (extractNodeCosts prelimStatesCost) --thisNodeCosts
                        sumThreeCosts
                in --should this be reversed so tail recursive?
                (V.singleton thisNode) V.++ (leftResult V.++ rightResult)
                )
                

-- | getPrevName retrieves the name of a given code from the previousBinaryTree 
--or "firstTree" if previous tree is empty (first one)
getPrevName :: PhyloComponent -> Int -> String
getPrevName binaryTree nodeCode = 
    if V.null binaryTree then "firstTree"
    else nodeName (binaryTree V.! nodeCode)


-- | extractNodeCosts creates list of costs from list of pairs of cost, states
extractNodeCosts :: [(BaseChar, Float)] -> V.Vector Float
extractNodeCosts inPair =
    if null inPair then V.empty
    else 
        let (_, b) = head inPair
        in
        V.cons b (extractNodeCosts (tail inPair))

extractNodeStates :: [(BaseChar, Float)] -> CharacterSetList
extractNodeStates inPair =
    if null inPair then []
    else 
        let (a, _) = head inPair
        in
        a : (extractNodeStates (tail inPair))


-- | getRootCode scans PhyloComponent for root node starting with nodeNum and returns 
-- root index
getRootCode :: PhyloComponent -> Int -> Int
getRootCode inTree nodeNum =
    if nodeNum == V.length inTree then error ("Error: No root found in " ++ show inTree)
    else 
       if isRoot $ inTree V.! nodeNum then nodeNum
       else getRootCode inTree (nodeNum + 1)

-- | binaryToNewick takes a binary tree and outputs string representation in
-- newick format. Calls binaryToNewickNames
binaryToNewick :: [PhyloComponent] -> [String]
binaryToNewick binTreeList =
    if null binTreeList then []
    else 
        let binTree = head binTreeList
            inRootCode = getRootCode binTree 0
            nameTree =  binaryToNewickNames binTree (binTree V.! inRootCode)
            newRootCode = getRootCode nameTree 0
        in
        (nodeName (nameTree V.! newRootCode)) : binaryToNewick (tail binTreeList)

-- | rootModifyName adds parens and semicolon to name if root for single
-- descendent
rootModifyName :: String -> Bool -> String
rootModifyName inName isRoot =
    if null inName then error "Name list empty"
    else 
        if isRoot then
            ("(" ++ inName ++ ");")
        else inName

-- | addColonIfRoot suprisingly enough, adds acolon to end of name if root for
-- valid newick format
addColonIfRoot :: Bool -> String -> String
addColonIfRoot isRoot inName =
    if null inName then error "Name list empty"
    else 
        if isRoot then
            (inName ++ ";")
        else inName


-- | binaryToNewickNames takes a binary tree and outputs tree obnly with names
-- updated.  Based on `traverseComponen' 
binaryToNewickNames ::  PhyloComponent -> PhyloNode -> PhyloComponent
binaryToNewickNames inComp curPNode
  | not (isTreeNode curPNode) =
    error ("Should not hit network node in traversal" ++ show curPNode)
  | length (children curPNode) > 2 =
    error "Descendant polytomies not yet implemented"
  | length (children curPNode) == 1 =
    let onlyNodeCode = head (children curPNode)
        onlyResult = binaryToNewickNames inComp (inComp V.! onlyNodeCode)
        thisName = rootModifyName (nodeName $ V.head onlyResult) (isRoot curPNode) --(inComp V.! onlyNodeCode)) (isRoot curPNode)
        thisNode = modifyNodeName curPNode thisName
    in   --add as node name?  helpful to follow rsolutions 
        --onlyResult V.++ V.singleton thisNode
        (V.singleton thisNode) V.++ onlyResult
  | isTerminal curPNode = 
    V.singleton curPNode
  | otherwise = 
    let leftNodeCode = head (children curPNode)
        rightNodeCode = last (children curPNode)
        leftResult
               = binaryToNewickNames inComp (inComp V.! leftNodeCode) 
        rightResult
               = binaryToNewickNames inComp (inComp V.! rightNodeCode) 
        thisName --check here for already done in previous rootings/trees, should control for left/right name issues
               = addColonIfRoot (isRoot curPNode) ("(" ++  
                   (min (nodeName (V.head leftResult))  (nodeName (V.head rightResult))) ++
                     "," ++ (max (nodeName (V.head leftResult))  (nodeName (V.head rightResult)))  ++ ")")
        thisNode = modifyNodeName curPNode thisName
    in
        --should this be reversed so tail recursive?
        --(leftResult V.++ rightResult) V.++ V.singleton thisNode
        (V.singleton thisNode) V.++ (leftResult V.++ rightResult)

-- | getInEdges takes a node and its parent(s) and return list of edges 
-- (min code, max code)
getInEdges :: [Int] -> Int -> [(Int, Int)]
getInEdges parents curNode =
    if null parents then []
    else 
        let curParent = head parents
            inNode = min curParent curNode
            outNode = max curParent curNode
        in 
        (inNode, outNode) : (getInEdges (tail parents) curNode) 
        

-- | edgeSetFromCopmonent takes a phylogenetic component and returns edge set
-- generates them as edges leading to node
edgeSetFromComponent :: PhyloComponent -> Set.Set (Int, Int)
edgeSetFromComponent inComponent =
    if V.null inComponent then Set.empty
    else 
        let curNode = V.head inComponent
            parentList = parents curNode
            inEdgeList = getInEdges parentList (code curNode)    
        in
        Set.union (Set.fromList inEdgeList) (edgeSetFromComponent $ V.tail inComponent) 


-- | edgeSetFromCopmonentList takes a list of phylogenetic components and returns edge set
-- generates them as edges leading to node
edgeSetFromComponentList :: [PhyloComponent] -> Set.Set (Int, Int)
edgeSetFromComponentList inComponentList =
    if null inComponentList then Set.empty
    else 
        let curComponent = head inComponentList
        in
        Set.union (edgeSetFromComponent curComponent) (edgeSetFromComponentList $ tail inComponentList) 
        
-- | edgeSetFromComponentListSome generate edge list from list of components,
-- but excludes those not in input list of Ints 
-- generates them as edges leading to node
edgeSetFromComponentListSome :: [PhyloComponent] -> [Int] -> Int -> Set.Set (Int, Int)
edgeSetFromComponentListSome inComponentList onlyThese thisNum =
    if null inComponentList || null onlyThese then Set.empty
    else
        if elem thisNum onlyThese then
            Set.union (edgeSetFromComponent $ head inComponentList) (edgeSetFromComponentListSome (tail inComponentList) onlyThese (thisNum + 1))
        else
            edgeSetFromComponentListSome (tail inComponentList) onlyThese (thisNum + 1)


-- | listOfVectorVector takes Vector.Vector (Vector.Vector Int) and
-- return [Int]
listOfVector :: [V.Vector Int] -> [Int]
listOfVector inVectList =
    if null inVectList then []
    else 
        let first = head inVectList
            firstList = V.toList first 
        in 
        firstList ++ (listOfVector (tail inVectList))



