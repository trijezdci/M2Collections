(* Modula-2 Collections Library
 *
 *  @file AAT.mod
 *  AAT implementation
 *
 *  Universal AA Tree
 *
 *  Author: Benjamin Kowarsch
 *
 *  Copyright (C) 2010 Benjamin Kowarsch. All rights reserved.
 *
 *  License:
 *
 *  Redistribution  and  use  in source  and  binary forms,  with  or  without
 *  modification, are permitted provided that the following conditions are met
 *
 *  1) NO FEES may be charged for the provision of the software.  The software
 *     may  NOT  be published  on websites  that contain  advertising,  unless
 *     specific  prior  written  permission has been obtained.
 *
 *  2) Redistributions  of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimer.
 *
 *  3) Redistributions  in binary form  must  reproduce  the  above  copyright
 *     notice,  this list of conditions  and  the following disclaimer  in the
 *     documentation and other materials provided with the distribution.
 *
 *  4) Neither the author's name nor the names of any contributors may be used
 *     to endorse  or  promote  products  derived  from this software  without
 *     specific prior written permission.
 *
 *  5) Where this list of conditions  or  the following disclaimer, in part or
 *     as a whole is overruled  or  nullified by applicable law, no permission
 *     is granted to use the software.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING,  BUT NOT LIMITED TO,  THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY  AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT  SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
 * LIABLE  FOR  ANY  DIRECT,  INDIRECT,  INCIDENTAL,  SPECIAL,  EXEMPLARY,  OR
 * CONSEQUENTIAL  DAMAGES  (INCLUDING,  BUT  NOT  LIMITED  TO,  PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES;  LOSS OF USE,  DATA,  OR PROFITS; OR BUSINESS
 * INTERRUPTION)  HOWEVER  CAUSED  AND ON ANY THEORY OF LIABILITY,  WHETHER IN
 * CONTRACT,  STRICT LIABILITY,  OR TORT  (INCLUDING NEGLIGENCE  OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,  EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 *  
 *)


(* ---------------------------------------------------------------------------
 * Reference documents
 * ---------------------------------------------------------------------------
 *
 * AA trees:  http://user.it.uu.se/~arnea/ps/simp.pdf
 * Sentinel search:  http://user.it.uu.se/~arnea/ps/searchproc.pdf *)


IMPLEMENTATION (* OF *) MODULE AAT;

FROM SYSTEM IMPORT ADR, ADDRESS, TSIZE;
FROM Storage IMPORT ALLOCATE, DEALLOCATE;


TYPE

(* ---------------------------------------------------------------------------
 * private type:  Node
 * ------------------------------------------------------------------------ *)

    NodePtr = POINTER TO Node;
    
    Node = RECORD
        level : CARDINAL;
        key   : Key;
        value : DataPtr;
        left,
        right : NodePtr;
    END; (* NodeDescriptor *)


(* ---------------------------------------------------------------------------
 * type implementation:  AAT.Tree
 * ------------------------------------------------------------------------ *)

    Tree = POINTER TO TreeDescriptor;
    
    TreeDecriptor = RECORD
        entryCount : Capacity;
        root       : NodePtr;
    END; (* TreeDescriptor *)


VAR

(* ---------------------------------------------------------------------------
 * tree node sentinel representing the bottom of a tree
 * ------------------------------------------------------------------------ *)

    bottom : NodePtr;  (* = ^sentinel *)
    sentinel : Node;  (* = { 0, 0, NIL, bottom, bottom } *)


(* ---------------------------------------------------------------------------
 * temporary node pointers for use during node removal
 * ------------------------------------------------------------------------ *)

    previousNode,
    candidateNode : NodePtr;  (* both initialised to NIL *)


(* ---------------------------------------------------------------------------
 * function:  AAT.new ( status )
 * ---------------------------------------------------------------------------
 *
 * Creates  and returns  a new AA tree object.  Returns NIL if the tree object
 * could not be created.
 *
 * The status of the operation is passed back in <status>. *)

PROCEDURE new ( VAR status : Status ) : Tree;

VAR
    newTree : Tree;
    
BEGIN

    (* allocate new tree *)
    NEW(newTree);
    
    (* bail out if allocations failed *)
    IF newTree = NIL THEN
        status := allocationFailed;
        RETURN NIL;
    END; (* IF *)
    
    (* initialise new tree *)
    newTree^.entryCount := 0;
    newTree^.root := bottom;
    
    (* pass status and return new tree to caller *)
    RETURN newTree;
    
END new;


(* ---------------------------------------------------------------------------
 * function:  AAT.storeEntry( tree, key, value, status )
 * ---------------------------------------------------------------------------
 *
 * Stores <value> for <key>  in <tree>.  The new entry is added  by reference,
 * NO data is copied.  The function fails  if NIL is passed in  for <tree>  or
 * <value> or if zero is passed in for <key>.
 *
 * The status of the operation is passed back in <status>. *)

PROCEDURE storeEntry ( tree   : Tree;
                       key    : Key;
                       value  : DataPtr;
                   VAR status : Status );
VAR
    newRoot : NodePtr;

BEGIN
    
    (* bail out if tree is NIL *)
    IF tree = NIL THEN
        status := invalidTree;
        RETURN;
    END; (* IF *)
    
    (* bail out if value is NIL *)
    IF value = NIL THEN
        status := invalidData;
        RETURN;
    END; (* IF *)
    
    (* insert entry *)
    newRoot := insert(tree^.root, key, value, status);
    
    IF status = success THEN
        tree^.root := newRoot;
        INC(tree^.entryCount);
    END; (* IF *)
    
    RETURN;
    
END storeEntry;


(* ---------------------------------------------------------------------------
 * function:  AAT.valueForKey( tree, key, status )
 * ---------------------------------------------------------------------------
 *
 * Returns the value stored for <key< in <tree>.  If no value for <key> exists
 * in <tree> or if NIL is passed in for <tree> then NIL is returned.
 *
 * The status of the operation is passed back in <status>. *)

PROCEDURE valueForKey ( tree   : Tree;
                        key    : Key;
                    VAR status : Status ) : DataPtr;
VAR
    thisNode : NodePtr;

BEGIN
    
    (* bail out if tree is NIL *)
    IF tree = NIL THEN
        status := invalidTree;
        RETURN NIL;
    END; (* IF *)
    
    (* set sentinel's key to search key *)
    bottom->key := key;
    
    (* start at the root *)
    thisNode := tree^.root;
    
    (* search until key found or bottom of tree reached *)
    WHILE key # thisNode^.key DO
    
        (* move down left if key is less than key of current node *)
        IF key < this^.key THEN
            thisNode := thisNode^.left;
        
        (* move down right if key is greater than key of current node *)
        ELSIF key > this^.key THEN
            thisNode := thisNode^.right;
        END; (* IF *)
    
    END; (* WHILE *)
    
    (* reset sentinel's key *)
    bottom^.key = 0;
    
    (* check whether or not bottom has been reached *)
    IF thisNode # bottom THEN
        status := success;
        RETURN thisNode^.value;
    ELSE (* bottom reached means key not found *)
        status := entryNotFound;
        RETURN NIL;
    END; (* IF *)

END valueForKey;


(* ---------------------------------------------------------------------------
 * function:  AAT.removeEntry( tree, key, status )
 * ---------------------------------------------------------------------------
 *
 * Removes the entry stored for <key> from <tree>.  The function fails  if NIL
 * is passed in for <tree> or if no entry for <key> is stored in <tree>.
 *
 * The status of the operation is passed back in <status>. *)

PROCEDURE removeEntry ( tree   : Tree;
                        key    : Key;
                    VAR status : Status );
VAR
    newRoot : NodePtr;

BEGIN

    (* bail out if tree is NIL *)
    IF tree = NIL THEN
        status := invalidTree;
        RETURN;
    END; (* IF *)
    
    (* remove entry *)
    newRoot := remove(tree^.root, key, status);
    
    IF status = success THEN
        tree^.root := newRoot;
        DEC(tree^.entryCount);
    END; (* IF *)
    
    RETURN;

END removeEntry;


(* ---------------------------------------------------------------------------
 * function:  AAT.capacity( tree )
 * ---------------------------------------------------------------------------
 *
 * Returns the total capacity of tree <tree>,  returns  zero  if NIL is passed
 * in for <tree>. *)

PROCEDURE capacity ( tree : Tree ) : Capacity;

BEGIN

    (* bail out if tree is NIL *)
    IF tree = NIL THEN
        RETURN 0;
    END; (* IF *)
    
    RETURN tree^.entryCount;

END capacity;


(* ---------------------------------------------------------------------------
 * function:  AAT.entryCount( tree )
 * ---------------------------------------------------------------------------
 *
 * Returns the number of entries stored in tree <tree>,  returns  zero  if NIL
 * is passed in for <tree>. *)

PROCEDURE entryCount ( tree : Tree ) : Capacity;

BEGIN

    (* bail out if tree is NIL *)
    IF tree = NIL THEN
        RETURN 0;
    END; (* IF *)
    
    RETURN tree^.entryCount;

END entryCount;


(* ---------------------------------------------------------------------------
 * function:  AAT.isResizable( tree )
 * ---------------------------------------------------------------------------
 *
 * Returns TRUE  if the  capacity of <tree>  can change  after <tree> has been
 * instatiated,  returns FALSE otherwise. *)

PROCEDURE isResizable ( tree : Tree ) : BOOLEAN;

BEGIN
    RETURN TRUE; (* this is a dynamic data structure *)
END isResizable;


(* ---------------------------------------------------------------------------
 * function:  AAT.dispose( tree )
 * ---------------------------------------------------------------------------
 *
 * Disposes of tree object <tree> and returns NIL. *)

PROCEDURE dispose ( VAR tree : Tree ) : Tree;

BEGIN

    IF tree # NIL THEN
    
        (* deallocate all nodes *)
        removeAll(tree^.root);
        
        (* deallocate descriptor *)
        DISPOSE(tree);
    
    END; (* IF *)
    
    RETURN NIL;

END dispose;


(* ===========================================================================
 * P r i v a t e   F u n c t i o n s   a n d   P r o c e d u r e s
 * ======================================================================== *)

(* ---------------------------------------------------------------------------
 * private function:  skew( node )
 * ---------------------------------------------------------------------------
 *
 * Rotates <node> to the right if its left child has the same level as <node>.
 * Returns the new root node.  NIL must not be passed in for <node>. *)
 
PROCEDURE skew ( node : NodePtr ) : NodePtr;

VAR
    tempNode : NodePtr;
    
BEGIN

    (* rotate right if left child has same level *)
    IF node^.level = node^.left^.level THEN
        tempNode := node;
        node := node^.left;
        tempNode^.left := node^.right;
        node^.right := tempNode;
    END; (* IF *)
    
    RETURN node;
    
END skew;


(* ---------------------------------------------------------------------------
 * private function:  split( node )
 * ---------------------------------------------------------------------------
 *
 * Rotates <node> left and promotes the level of its right child to become its
 * new parent if <node> has two consecutive right children with the same level
 * level as <node>.  Returns the new root node.  NIL  must  not  be  passed in
 * for <node>. *)
 
PROCEDURE split ( node : NodePtr ) : NodePtr;

VAR
    tempNode : NodePtr;

BEGIN

    (* rotate left if there are two right children on same level *)
    IF node^level = node^.right^.right^.level THEN
        tempNode := node;
        node := node^.right;
        tempNode^.right := node^.left;
        node^.right := tempNode;
        INC(node^.level);
    END; (* IF *)
    
    RETURN node;

END split;


(* ---------------------------------------------------------------------------
 * private function:  insert( node, key, value, status )
 * ---------------------------------------------------------------------------
 *
 * Recursively inserts  a new entry for <key> with <value> into the tree whose
 * root node is <node>.  Returns the new root node  of the resulting tree.  If
 * allocation fails  or  if a node with the same key already exists,  then  NO
 * entry will be inserted and NIL is returned.
 *
 * The  status of the operation  is passed back  in <status>.  NIL must not be
 * passed in for <node>. *)

PROCEDURE insert ( node  : NodePtr;
                   key   : Key;
                   value : DataPtr;
              VAR status : Status ) : NodePtr;
VAR
    newNode : NodePtr;

BEGIN

    IF node = bottom THEN
    
        (* allocate a new node *)
        NEW(newNode);
        
        (* bail out if allocation failed *)
        IF newNode = NIL THEN
            status := allocationFailed;
            RETURN NIL;
        END; (* IF *)
        
        (* initialise the new node *)
        newNode^.level := 1;
        newNode^.key := key;
        newNode^.value := value;
        newNode^.left := bottom;
        newNode^.right := bottom;
        
        (* link the new node to the tree *)
        node := newNode;
    
    ELSIF node^.key > key THEN
    
        (* recursive insert left *)
        node := insert(node^.left, key, value, status);
        
        (* bail out if allocation failed *)
        IF status = allocationFailed THEN
            RETURN NIL;
        END; (* IF *)
    
    ELSIF node^.key < key THEN
    
        (* recursive insert right *)
        node := insert(node^.right, key, value, status);

        (* bail out if allocation failed *)
        IF status = allocationFailed THEN
            RETURN NIL;
        END; (* IF *)        
        
    ELSE (* key already exists *)
        
        (* bail out *)
        status := keyNotUnique;
        RETURN NIL;
    
    END; (* IF *)
    
    (* rebalance the tree *)
    node := skew(node);
    node := split(node);
    
    (* pass status and return the new root node to caller *)
    status := success;
    RETURN node;

END insert;


(* ---------------------------------------------------------------------------
 * private function:  remove( node, key, status )
 * ---------------------------------------------------------------------------
 *
 * Recursively searches the tree  whose root node is <node>  for a node  whose
 * key is <key> and if found,  removes that node  and rebalances the resulting
 * tree,  then  returns  the new root  of the resulting tree.  If no node with
 * <key> exists,  then NIL is returned.
 *
 * The  status of the operation  is passed back  in <status>.  NIL must not be
 * passed in for <node>. *)

PROCEDURE remove ( node : NodePtr; key : Key; VAR status : Status ) : NodePtr;

BEGIN

    (* bail out if bottom has been reached *)
    IF node = bottom THEN
        status := entryNotFound;
        RETURN NIL;
    END; (* IF *)
    
    (* move down recursively until key is found or bottom is reached *)
    
    previousNode := node;
    
    (* move left if search key is less than current node's key *)
    IF key < node^key THEN
        node := remove(node^.left, key, status);
    
    (* move right if search key is not less than current node's key *)
    ELSE
        candidateNode := node;
        node := remove(node^.right, key, status);
    END; (* IF *)
    
    (* remove entry *)
    IF node = previousNode AND
       candidateNode # bottom AND
       candidateNode^.key = key THEN
       
       candidateNode^.key := node^.key;
       candidateNode := bottom;
       node := node^.right;
       
       DISPOSE(previousNode);
       status := success;
       
    (* rebalance on the way back up *)
    ELSIF node^.level - 1 > node^.left^.level OR
          node^.level - 1 < node^.right^.level THEN
          
        DEC(node^.level);
        IF node^.level < node^.right^.level THEN
            node^.right^.level := node^.level;
        END; (* IF *)
        
        node := skew(node);
        node := skew(node^.right);
        node := skew(node^.right^.right);
        node := split(node);
        node := split(node^.right);
    
    END; (* IF *)
    
    RETURN node;

END remove;


(* ---------------------------------------------------------------------------
 * private procedure:  removeAll( node )
 * ---------------------------------------------------------------------------
 *
 * Recursively  removes  all nodes  from the tree  whose root node  is <node>.
 * NIL must not be passed in for <node>. *)
 
PROCEDURE removeAll ( node : NodePtr );

BEGIN

    (* bail out if already at the bottom *)
    IF node = bottom THEN
        RETURN;
    END; (* IF *)
    
    (* remove the left subtree *)
    removeAll(node^.left);
    
    (* remove the right subtree *)
    removeAll(node^.right);
    
    (* deallocated descriptor *)
    DISPOSE(node);
    
    RETURN;
    
END removeAll;


(* ---------------------------------------------------------------------------
 * module initialisation
 * ------------------------------------------------------------------------ *)

BEGIN

    bottom := ^sentinel;

    sentinel.level := 0;
    sentinel.key := 0;
    sentinel.value := NIL;
    sentinel.left := bottom;
    sentinel.right := bottom;
    
    previousNode := NIL;
    candidateNode := NIL;
 
END AAT.
