type 'a tree =
  | Node of 'a tree * 'a * 'a tree
  | Leaf

let rec tree_fold f init tree = match tree with
	|Leaf -> init
	|Node(left, value, right) ->
		let left_acc = tree_fold f init left in 
		let right_acc = tree_fold f init right in
		f left_acc value right_acc 


let map tree f = 
	tree_fold (fun left value right -> Node(left, f value, right)) Leaf tree
 
let mirror tree = 
	tree_fold (fun left value right -> Node(right, value, left)) Leaf tree

let in_order tree = 
 	tree_fold (fun left value right -> left @[value] @ right) [] tree
	
let pre_order tree =
	tree_fold (fun left value right -> [value] @ left @ right) [] tree

let compose tree = 
	tree_fold (fun left value right x -> right(value(left x))) (fun x -> x) tree

let depth tree = 
	let helper left value right =  
		if left > right then 
			left + 1
		else
			right + 1
	in
	tree_fold (fun left value right -> 
		if left > right then
                        left + 1
                else
                        right + 1) 0 tree

(* Assume complete tree *)
let trim tree n = match tree with 
	|Leaf -> Leaf
	|Node(left, value, right) -> Node(
		(tree_fold (fun l v r ->
		if (depth tree) - (depth l) < n then 
			Node(l, v, r) 
		else 
			Leaf) Leaf left), 

value, 



tree_fold (fun l v r ->
                if (depth tree) - (depth r) < n then   
                        Node(l, v, r)           
                else
                        Leaf) Leaf right)

let rec split lst v acc = match lst with 
	|[] -> (match acc with 
		|[] -> ([], [])
		|h::t -> (acc, []))
	|h::t -> 
		if h = v then 
			(acc, t)
		else 
			split t v (acc @ [h]) 

let rec must_contain (og: 'a list) (filter: 'a list) (acc: 'a list) = match og with
        |[] -> []
        |h::t ->
                if List.mem h filter = true then
                        acc @ [h] @ must_contain t filter acc
                else 
                        acc @ must_contain t filter acc

let rec from_pre_in pre in_ord = match pre with 
	|[] -> Leaf
	|h::t -> 
		let (left, right) = split in_ord h [] in
		let left_helper = from_pre_in (must_contain pre left []) left in 
		let right_helper = from_pre_in (must_contain pre right []) right in
		Node(left_helper, h, right_helper) 
		
			
