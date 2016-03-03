type BinTree<'a> =
  |EmptyTree
  |Node of 'a * BinTree<'a> * BinTree<'a>

//gives a string that visulaizes the tree data structure
let rec stringRep aTree indent =
    match aTree with
    | EmptyTree -> ""
    | Node (value, leftChild, rightChild) -> 
        let rightString = stringRep rightChild (indent+"\t")
        let thisString =  indent + (string value) + "\n"
        let leftString = stringRep leftChild (indent+"\t")
        rightString + thisString + leftString

//Creates an instance of a tree
let t1 = Node( 16, 
                Node(8, 
                    Node(3, EmptyTree, EmptyTree),
                    Node(15, EmptyTree, EmptyTree)
                ), 
                Node(20,
                    Node(18, EmptyTree, EmptyTree),
                    Node(22,
                        Node(21, EmptyTree, EmptyTree),
                        Node(24, EmptyTree, EmptyTree)
                    )
                )
            )
printf "-- t1 --------------------\n%s" (stringRep t1 "")

// gives a new tree made from adding the value given to the old tree
// throws an exception if you try to add a value already in the tree
let rec bstAdd valToAdd aTree = 
    match aTree with 
    | EmptyTree -> 
        Node (valToAdd, EmptyTree, EmptyTree)
    | Node (value, _, _) when value = valToAdd -> 
        failwith "Can't add the same value to a BST twice"
    | Node (value, leftChild, rightChild) when value > valToAdd -> 
        let newLeftSubTree = bstAdd valToAdd leftChild 
        Node(value, newLeftSubTree, rightChild)
    | Node (value, leftChild, rightChild) -> 
        let newRightSubTree = bstAdd valToAdd rightChild 
        Node(value, leftChild, newRightSubTree)

let t2 = (bstAdd 21 (bstAdd 3 (bstAdd 24 (bstAdd 22 (bstAdd 15 (bstAdd 18 (bstAdd 20 (bstAdd 8 (bstAdd 16 EmptyTree )))))))))
printf "-- t2 --------------------\n%s" (stringRep t2 "")

let t3 = 
    EmptyTree 
    |> bstAdd 16 
    |> bstAdd 8 
    |> bstAdd 20 
    |> bstAdd 18 
    |> bstAdd 15
    |> bstAdd 22
    |> bstAdd 24
    |> bstAdd 3
    |> bstAdd 21
printf "-- t3 --------------------\n%s" (stringRep t3 "")

//evaluates to true if the given value is in the Binary search tree, false otherwise
let rec bstFind bstTree valToFind =
    match bstTree with
    | Node(value, _, _) when value = valToFind -> true
    | Node(value, _, rightChild) when value < valToFind -> bstFind rightChild valToFind
    | Node(value, leftChild, _) -> bstFind leftChild valToFind //value > valToFind
    | EmptyTree -> false

//gives a new binary search tree made from adding all values in the list to the given bst
//non-tail recursive
let rec addListToBST bstTree listToAdd =
    match listToAdd with
    | head::tail -> addListToBST (bstAdd head bstTree) tail
    | [] -> bstTree

//gives a list of all values from the bst in sorted order
//Note: the @ operator can be used to combine two lists
let rec bstToSortedList bstTree =
    match bstTree with
    |Node(value, leftChild, rightChild) ->
        (bstToSortedList leftChild) @ value::(bstToSortedList rightChild)
    |EmptyTree -> []



//test code
let valueList = [1;2;4;14]
//search for values in tree. should be false, false, true, true
printf "%A\n" (bstFind t1 valueList.[0])
printf "%A\n" (bstFind t1 valueList.[1])
printf "%A\n" (bstFind t1 3)
printf "%A\n" (bstFind t1 22)

//add valueList, then search again. should be all trues.
let t4 = addListToBST t1 valueList
printf "%A\n" (bstFind t4 valueList.[0])
printf "%A\n" (bstFind t4 valueList.[1])
printf "%A\n" (bstFind t4 valueList.[2])
printf "%A\n" (bstFind t4 valueList.[3])

//t4 should have new vals in the tree, while t1, t2, and t3 not.
printf "%A\n" (bstToSortedList t4)
printf "%A\n" (bstToSortedList t1)
printf "%A\n" (bstToSortedList t2)
printf "%A\n" (bstToSortedList t3)
