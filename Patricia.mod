(* Modula-2 Collections Library
 *
 *  @file Patricia.mod
 *  Patricia implementation
 *
 *  Universal Patricia Trie
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


IMPLEMENTATION (* OF *) MODULE Patricia;

FROM SYSTEM IMPORT ADR, ADDRESS, TSIZE;


TYPE

(* ---------------------------------------------------------------------------
 * private type:  Node
 * ------------------------------------------------------------------------ *)

    NodePtr = POINTER TO Node;
    
    Node = RECORD
        index : CARDINAL;
        key   : ADDRESS;
        value : DataPtr;
        left,
        right : NodePtr;
    END; (* NodeDescriptor *)


(* ---------------------------------------------------------------------------
 * type implementation:  Patricia.Trie
 * ------------------------------------------------------------------------ *)

    Trie = POINTER TO TrieDescriptor;
        entryCount : Capacity;
        root       : NodePtr
    TrieDescriptor = RECORD
        
    END; (* TrieDescriptor *)


(* ---------------------------------------------------------------------------
 * Trie Action Handler type
 * ---------------------------------------------------------------------------
 *
 * Action handlers are  called by function forEachEntryDo  to report key/value
 * pairs that match the search criteria passed to the forEachEntryDo function.
 *
 * Handlers take two arguments <key> and <value>:
 *
 *  o  <key> is a key that matches the search criteria
 *  o  <value> is the value stored for key <key> in the searched trie *)

    ActionHandler =
        PROCEDURE ( (* key *) ARRAY OF CHAR, (* value *) DataPtr );


VAR

(* ---------------------------------------------------------------------------
 * tree node sentinel representing the bottom of a trie
 * ------------------------------------------------------------------------ *)

    bottom : NodePtr;  (* = ^sentinel *)
    sentinel : Node;  (* = { 0, NIL, NIL, bottom, bottom } *)


(* ---------------------------------------------------------------------------
 * function:  Patricia.new ( status )
 * ---------------------------------------------------------------------------
 *
 * Creates  and  returns  a  new trie object.  Returns NIL  if the trie object
 * could not be created.
 *
 * The status of the operation is passed back in <status>. *)

PROCEDURE new ( VAR status : Status ) : Queue;

BEGIN
    (* TO DO *)
END new;


(* ---------------------------------------------------------------------------
 * function:  Patricia.storeEntry( trie, key, value, status )
 * ---------------------------------------------------------------------------
 *
 * Stores <value> for <key>  in <trie>.  The new entry is added  by reference,
 * NO data is copied.  The function fails  if NIL is passed in  for <trie>  or
 * <key> or if a zero length string is passed in for <key>.
 *
 * The status of the operation is passed back in <status>. *)

PROCEDURE storeEntry ( trie   : Trie;
                       key    : ARRAY OF CHAR;
                       value  : DataPtr;
                   VAR status : Status );

BEGIN
    (* TO DO *)
END storeEntry;


(* ---------------------------------------------------------------------------
 * function:  Patricia.replaceEntry( trie, key, value, status )
 * ---------------------------------------------------------------------------
 *
 * Searches for  an entry in <trie>  whose key  exactly  matches <key> and re-
 * places its value with <value>.  The function fails  if NIL is passed in for
 * <trie> or <key>,  or  if a  zero length string  is passed in for <key>,  or
 * if no entry is found in <trie> with a key that exactly matches <key>.
 *
 * The status of the operation is passed back in <status>. *)

PROCEDURE replaceEntry ( trie   : Trie;
                         key    : ARRAY OF CHAR;
                         value  : DataPtr;
                     VAR status : Status );

BEGIN
    (* TO DO *)
END replaceEntry;


(* ---------------------------------------------------------------------------
 * function:  Patricia.valueForKey( trie, key, status )
 * ---------------------------------------------------------------------------
 *
 * Returns the value stored for <key< in <trie>.  If no value for <key> exists
 * in <trie> or if NIL is passed in for <trie> then NIL is returned.
 *
 * The status of the operation is passed back in <status>. *)

PROCEDURE valueForKey ( trie   : Trie;
                        key    : ARRAY OF CHAR;
                    VAR status : Status ) : DataPtr;

BEGIN
    (* TO DO *)
END valueForKey;


(* ---------------------------------------------------------------------------
 * function:  Patricia.forEachEntryDo( trie, prefix, action, status )
 * ---------------------------------------------------------------------------
 *
 * Traverses <trie>  visiting all entries whose keys have a common prefix with
 * <prefix>  and invokes the  action handler  passed in for <action>  for each
 * entry visited.  If an  empty string  is passed in  for <prefix>  then every
 * entry in <trie> will be visited.  The function returns  the total number of
 * entries visited.  The function fails  and returns zero  if NIL is passed in
 * in for <trie> or <action>.
 *
 * Each time <action> is called,  the following parameters are passed to it:
 *
 *  o  first parameter :  the key of the visited node
 *  o  second parameter:  the value of the visited node
 *
 * The status of the operation is passed back in <status>. *)

PROCEDURE forEachEntryDo ( trie   : Trie;
                           prefix : ARRAY OF CHAR;
                           action : ActionHandler;
                       VAR status : Status ) : DataPtr;

BEGIN
    (* TO DO *)
END forEachEntryDo;


(* ---------------------------------------------------------------------------
 * function:  Patricia.numberOfEntriesWithPrefix( trie, prefix )
 * ---------------------------------------------------------------------------
 *
 * Returns  the  number of entries  stored in <trie>  whose keys have a common
 * prefix with <prefix>.  If an  empty string is passed in for <prefix>,  then
 * the  total number  of entries  stored in <trie>  is returned.  The function
 * fails and returns zero if NULL is passed in for <trie> or <key>. *)

PROCEDURE numberOfEntriesWithPrefix ( trie   : Trie;
                                      prefix : ARRAY OF CHAR ) : Capacity;

BEGIN
    (* TO DO *)
END numberOfEntriesWithPrefix;


(* ---------------------------------------------------------------------------
 * function:  Patricia.removeEntry( trie, key, status )
 * ---------------------------------------------------------------------------
 *
 * Removes the entry stored for <key> from <trie>.  The function fails  if NIL
 * is passed in for <trie> or if no entry for <key> is stored in <trie>.
 *
 * The status of the operation is passed back in <status>. *)

PROCEDURE removeEntry ( trie   : Tree;
                        key    : ARRAY OF CHAR;
                    VAR status : Status );

BEGIN
    (* TO DO *)
END removeEntry;


(* ---------------------------------------------------------------------------
 * function:  Patricia.capacity( trie )
 * ---------------------------------------------------------------------------
 *
 * Returns the total capacity of trie <trie>,  returns  zero  if NIL is passed
 * in for <trie>. *)

PROCEDURE capacity ( trie : Trie ) : Capacity;

BEGIN
    (* TO DO *)
END capacity;


(* ---------------------------------------------------------------------------
 * function:  Patricia.entryCount( trie )
 * ---------------------------------------------------------------------------
 *
 * Returns the number of entries stored in trie <trie>,  returns  zero  if NIL
 * is passed in for <trie>. *)

PROCEDURE entryCount ( trie : Trie ) : Capacity;

BEGIN
    (* TO DO *)
END entryCount;


(* ---------------------------------------------------------------------------
 * function:  Patricia.isResizable( trie )
 * ---------------------------------------------------------------------------
 *
 * Returns TRUE  if the  capacity of <trie>  can change  after <trie> has been
 * instatiated,  returns FALSE otherwise. *)

PROCEDURE isResizable ( trie : Trie ) : BOOLEAN;

BEGIN
    RETURN TRUE
END isResizable;


(* ---------------------------------------------------------------------------
 * function:  Patricia.dispose( trie )
 * ---------------------------------------------------------------------------
 *
 * Disposes of tree object <trie> and returns NIL. *)

PROCEDURE dispose ( trie : Trie ) : Trie;

BEGIN
    (* TO DO *)
END dispose;


(* ---------------------------------------------------------------------------
 * module initialisation
 * ------------------------------------------------------------------------ *)

BEGIN

    bottom := ^sentinel;

    sentinel.level := 0;
    sentinel.key := NIL;
    sentinel.value := NIL;
    sentinel.left := bottom;
    sentinel.right := bottom;

END Patricia.
