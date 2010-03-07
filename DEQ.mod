(* M2DEQ
 *
 *  @file DEQ.mod
 *  M2DEQ interface
 *
 *  <description>
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


IMPLEMENTATION (* OF *) MODULE DEQ;


CONST

(* ---------------------------------------------------------------------------
// 
// ---------------------------------------------------------------------------
*)


TYPE

(* ---------------------------------------------------------------------------
// 
// ---------------------------------------------------------------------------
*)


(* ---------------------------------------------------------------------------
 * function:  DEQ.new ( status )
 * ---------------------------------------------------------------------------
 *
 * Creates  and returns a new queue object.  Returns  NIL  if the queue object
 * could not be created.
 *
 * The status of the operation is passed back in <status>. *)

PROCEDURE new ( VAR status : Status ) : Queue;

BEGIN
    (* TO DO *)
END new;


(* ---------------------------------------------------------------------------
 * function:  DEQ.prepend( queue, value, status )
 * ---------------------------------------------------------------------------
 *
 * Prepends  a new  entry <value>  at  the  head of <queue>.  The new entry is
 * added by reference,  no data is copied.  No entry is added if NIL is passed
 * in for <queue> or <value>.
 *
 * The status of the operation is passed back in <status>. *)

PROCEDURE prepend ( queue : Queue; value : DataPtr; VAR status : Status );

BEGIN
    (* TO DO *)
END prepend;


(* ---------------------------------------------------------------------------
 * function:  DEQ.prepend( queue, value, status )
 * ---------------------------------------------------------------------------
 *
 * Appends  a  new  entry <value>  at  the  tail of <queue>.  The new entry is
 * added by reference,  no data is copied.  No entry is added if NIL is passed
 * in for <queue> or <value>.
 *
 * The status of the operation is passed back in <status>. *)

PROCEDURE append ( queue : Queue; value : DataPtr; VAR status : Status );

BEGIN
    (* TO DO *)
END append;


(* ---------------------------------------------------------------------------
 * function:  DEQ.firstEntry( queue, status )
 * ---------------------------------------------------------------------------
 *
 * Removes  the first entry  from the head of <queue>  and returns it.  If the
 * queue is empty or NIL is passed in for <queue>,  then NIL is returned.
 *
 * The status of the operation is passed back in <status>. *)

PROCEDURE firstEntry ( queue : Queue; VAR status : Status ) : DataPtr;

BEGIN
    (* TO DO *)
END firstEntry;


(* ---------------------------------------------------------------------------
 * function:  DEQ.lastEntry( queue, status )
 * ---------------------------------------------------------------------------
 *
 * Removes  the last entry  from the tail of <queue>  and  returns it.  If the
 * queue is empty or NIL is passed in for <queue>,  then NIL is returned.
 *
 * The status of the operation is passed back in <status>. *)

PROCEDURE lastEntry ( queue : Queue; VAR status : Status ) : DataPtr;

BEGIN
    (* TO DO *)
END lastEntry;


(* ---------------------------------------------------------------------------
 * function:  DEQ.capacity( queue )
 * ---------------------------------------------------------------------------
 *
 * Returns the total capacity of queue <queue>,  returns zero if NIL is passed
 * in for <queue>. *)

PROCEDURE capacity ( queue : Queue ) : Capacity;

BEGIN
    (* TO DO *)
END capacity;


(* ---------------------------------------------------------------------------
 * function:  DEQ.entryCount( queue )
 * ---------------------------------------------------------------------------
 *
 * Returns the number of entries stored in queue <queue>,  returns zero if NIL
 * is passed in for <queue>. *)

PROCEDURE entryCount ( queue : Queue ) : Capacity;

BEGIN
    (* TO DO *)
END entryCount;


(* ---------------------------------------------------------------------------
 * function:  DEQ.isResizable( queue )
 * ---------------------------------------------------------------------------
 *
 * Returns TRUE  if the  capacity of <queue> can change after <queue> has been
 * instatiated,  returns FALSE otherwise. *)

PROCEDURE isResizable ( queue : Queue ) : BOOLEAN;

BEGIN
    RETURN TRUE
END new;


(* ---------------------------------------------------------------------------
 * function:  DEQ.newIterator ( queue, status )
 * ---------------------------------------------------------------------------
 *
 * Creates and returns  a new iterator object  for iterating entries  in queue
 * <queue>.  Returns NIL if <queue> is NIL or empty.
 *
 * The status of the operation is passed back in <status>. *)

PROCEDURE newIterator ( queue : Queue; VAR status : Status ) : Iterator;

BEGIN
    (* TO DO *)
END newIterator;


(* ---------------------------------------------------------------------------
 * function:  DEQ.iterateNext( queue )
 * ---------------------------------------------------------------------------
 *
 * The  first  call  to this function  returns the  first  entry  of the queue
 * associated with <iterator>.  Subsequent calls return the queue's respective
 * successor entries.  Returns  NIL  if  any preceding call  returned the last
 * entry of the queue or if NIL is passed in for <iterator>.
 *
 * The status of the operation is passed back in <status>. *)

PROCEDURE iterateNext ( iterator : Iterator; VAR statusStatus ) : DataPtr;

BEGIN
    (* TO DO *)
END iterateNext;


(* ---------------------------------------------------------------------------
 * function:  DEQ.disposeIterator ( iterator )
 * ---------------------------------------------------------------------------
 *
 * Disposes of iterator object <iterator> and returns NIL. *)

PROCEDURE disposeIterator ( VAR iterator : Iterator ) : Iterator;

BEGIN
    (* TO DO *)
END disposeIterator;


(* ---------------------------------------------------------------------------
 * function:  DEQ.dispose ( queue )
 * ---------------------------------------------------------------------------
 *
 * Disposes of queue object <queue> and returns NIL. *)

PROCEDURE dispose ( VAR queue : Queue ) : Queue;

BEGIN
    (* TO DO *)
END dispose;


END DEQ.
