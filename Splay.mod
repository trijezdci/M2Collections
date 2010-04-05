(* Modula-2 Collections Library
 *
 *  @file Splay.mod
 *  Splay implementation
 *
 *  Universal Splay Tree
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
 * Splay trees:  http://www.cs.cmu.edu/~sleator/papers/self-adjusting.pdf
 * Sentinel search:  http://user.it.uu.se/~arnea/ps/searchproc.pdf *)


IMPLEMENTATION (* OF *) MODULE Splay;

FROM SYSTEM IMPORT ADR, ADDRESS, TSIZE;
FROM Storage IMPORT ALLOCATE, DEALLOCATE;


TYPE

(* ---------------------------------------------------------------------------
 * private type:  Node
 * ------------------------------------------------------------------------ *)

    NodePtr = POINTER TO Node;
    
    Node = RECORD
        key   : Key;
        value : DataPtr;
        left,
        right : NodePtr;
    END; (* NodeDescriptor *)


(* ---------------------------------------------------------------------------
 * type implementation:  Splay.Tree
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
 * function:  Splay.new ( status )
 * ---------------------------------------------------------------------------
 *
 * Creates  and  returns  a  new  splay tree object.  Returns NIL  if the tree
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
 * function:  Splay.storeEntry( tree, key, value, status )
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
 * function:  Splay.valueForKey( tree, key, status )
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
 * function:  Splay.removeEntry( tree, key, status )
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
 * function:  Splay.capacity( tree )
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
 * function:  Splay.entryCount( tree )
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
 * function:  Splay.isResizable( tree )
 * ---------------------------------------------------------------------------
 *
 * Returns TRUE  if the  capacity of <tree>  can change  after <tree> has been
 * instatiated,  returns FALSE otherwise. *)

PROCEDURE isResizable ( tree : Tree ) : BOOLEAN;

BEGIN
    RETURN TRUE; (* this is a dynamic data structure *)
END isResizable;


(* ---------------------------------------------------------------------------
 * function:  Splay.dispose( tree )
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
 * private function:  splayTopDown( node )
 * ---------------------------------------------------------------------------
 *
 * Rearranges the tree  whose root is <node> such  that the node  whose key is
 * <key> moves to the top.  If no node with <key> is present in the tree, then
 * the node that would be its closest neighbour is moved to the top instead.
 *
 * Returns the new root node.  NIL must not be passed in for <node>. *)

PROCEDURE splayTopDown ( VAR node : NodePtr; key : Key ) : NodePtr;

VAR
    N : Node;
    tempNode, leftSubTree, rightSubTree : NodePtr;

BEGIN

    IF node = bottom THEN
        RETURN node;
    END; (* IF *)
    
    N.left = bottom;
    N.right = bottom;
    leftSubTree := N;
    rightSubTree := N;
    
    LOOP
        
        IF key < node^.key THEN
            
            (* done when bottom reached *)
            IF node^.left = bottom THEN
                EXIT; (* LOOP *)
            END; (* IF *);
            
            (* rotate right if key is less than current node's key *)
            IF key < node^.left^.key THEN
                tempNode := node^.left;
                node^.left := tempNode^.right;
                tempNode^.right := node;
                node := tempNode;
                
                (* done when bottom reached *)
                IF node^.left = bottom THEN
                    EXIT; (* LOOP *)
                END; (* IF *)
                
            END; (* IF *)
            
            (* link right *)
            leftSubTree^.left := node;
            rightSubTree^.left := node;
            node := node^.left;
        
        ELSIF key > node^.key THEN
            
            (* done when bottom reached *)
            IF node^.right = bottom THEN
                EXIT; (* LOOP *)
            END; (* IF *);
            
            (* rotate left if key is less than current node's key *)
            IF key < node^.right^.key THEN
                tempNode := node^.right;
                node^.right := tempNode^.left;
                tempNode^.left := node;
                node := tempNode;
                
                (* done when bottom reached *)
                IF node^.right = bottom THEN
                    EXIT; (* LOOP *)
                END; (* IF *)
            
            END; (* IF *)
            
            (* link left *)
            leftSubTree^.right := node;
            leftSubTree^.left := node;
            node := node^.right;
        
        ELSE (* key = node^.key *)
            EXIT; (* LOOP *)
        END; (* IF *)
    
    END; (* LOOP *)
    
    (* reassemble the tree *)
    leftSubTree^.right := node^.left;
    rightSubTree^.left := node^.right;
    node^.left := N.right;
    node^.right := N.left;
    
    RETURN node;
    
END splayTopDown;


(* ---------------------------------------------------------------------------
 * private function:  insert( node, key, value, status )
 * ---------------------------------------------------------------------------
 *
 * Recursively inserts  a new entry for <key> with <value> into the tree whose
 * root node is <node>.  Returns the new root node  of the resulting tree.  If
 * allocation fails  or  if a node with the same key already exists,  then  NO
 * entry will be inserted and the current root node is returned.
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

    node := splayTopDown(node, key);
    
    (* bail out if key already exists *)
    IF key =  node^.key THEN
        status := keyNotUnique;
        RETURN node;
    END; (* IF *)
    
    (* allocate a new node *)
    NEW(newNode);
    
    (* bail out if allocation failed *)
    IF newNode = NIL THEN
        status := allocationFailed;
        RETURN node;
    END; (* IF *)
    
    (* initialise the new node *)
    newNode^.key := key;
    newNode^.value := value;
    
    IF key < node^.key THEN
        newNode^.left := node^.left;
        newNode^.right := node;
        node^.left := bottom;
    ELSE (* key > node^.key *)
        newNode^.right := node^.right;
        newNode^.left := node;
        node^.right := bottom;
    END; (* IF *)
    
    (* pass status and return the new root node to caller *)
    status := success;
    RETURN newNode;

END insert;


(* ---------------------------------------------------------------------------
 * private function:  remove( node, key, status )
 * ---------------------------------------------------------------------------
 *
 * Searches the tree whose root node is <node>  for a node  whose key is <key>
 * and if found,  removes that node  and  rebalances the resulting tree,  then
 * returns the new root  of the resulting tree.  If no node with <key> exists,
 * then the current root node is returned.
 *
 * The  status of the operation  is passed back  in <status>.  NIL must not be
 * passed in for <node>. *)

PROCEDURE remove ( node : NodePtr; key : Key; VAR status : Status ) : NodePtr;

VAR
    newRoot : NodePtr;

BEGIN

    (* bail out if bottom has been reached *)
    IF node = bottom THEN
        status := entryNotFound;
        RETURN node;
    END; (* IF *)
    
    node := splayTopDown(node, key);
    
    (* bail out if key not found *)
    IF key # node^.key THEN
        status := entryNotFound;
        RETURN node;
    END; (* IF *)
    
    (* isolate the node to be removed *)
    IF node^.left = bottom THEN
        newRoot := node^.right;
    ELSE
        newRoot := splayTopDown(node, key);
        newRoot^.right := node^.right;
    END; (* IF *)
    
    (* deallocate the isolated node *)
    DISPOSE(node);
    
    (* pass status and return the new root node to caller *)
    status := success;
    RETURN newNode;

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

    sentinel.key := 0;
    sentinel.value := NIL;
    sentinel.left := bottom;
    sentinel.right := bottom;
 
END Splay.
