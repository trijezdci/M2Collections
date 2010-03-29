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

BEGIN
    (* TO DO *)
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

BEGIN
    (* TO DO *)
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

BEGIN
    (* TO DO *)
END removeEntry;


(* ---------------------------------------------------------------------------
 * function:  AAT.capacity( tree )
 * ---------------------------------------------------------------------------
 *
 * Returns the total capacity of tree <tree>,  returns  zero  if NIL is passed
 * in for <tree>. *)

PROCEDURE capacity ( tree : Tree ) : Capacity;

BEGIN
    (* TO DO *)
END capacity;


(* ---------------------------------------------------------------------------
 * function:  AAT.entryCount( tree )
 * ---------------------------------------------------------------------------
 *
 * Returns the number of entries stored in tree <tree>,  returns  zero  if NIL
 * is passed in for <tree>. *)

PROCEDURE entryCount ( tree : Tree ) : Capacity;

BEGIN
    (* TO DO *)
END entryCount;


(* ---------------------------------------------------------------------------
 * function:  AAT.isResizable( tree )
 * ---------------------------------------------------------------------------
 *
 * Returns TRUE  if the  capacity of <tree>  can change  after <tree> has been
 * instatiated,  returns FALSE otherwise. *)

PROCEDURE isResizable ( tree : Tree ) : BOOLEAN;

BEGIN
    RETURN TRUE
END isResizable;


(* ---------------------------------------------------------------------------
 * function:  AAT.dispose( tree )
 * ---------------------------------------------------------------------------
 *
 * Disposes of tree object <tree> and returns NIL. *)

PROCEDURE dispose ( VAR tree : Tree ) : Tree;

BEGIN
    (* TO DO *)
END dispose;


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
