(* Modula-2 Collections Library
 *
 *  @file KVS.mod
 *  Key/Value Storage implementation
 *
 *  Universal Associative Array
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
 *) (*!m2pim*)

IMPLEMENTATION MODULE KVS;

FROM SYSTEM IMPORT ADDRESS, ADR, TSIZE;
FROM Storage IMPORT ALLOCATE, DEALLOCATE;


TYPE

(* ---------------------------------------------------------------------------
 * private type:  Entry
 * ------------------------------------------------------------------------ *)

    EntryPtr = POINTER TO Entry;
    
    Entry = RECORD
        key              : Key;
        value            : DataPtr;
        size             : CARDINAL;
        next             : EntryPtr;
        refCount         : CARDINAL;
        zeroTerminated,
        markedForRemoval : BOOLEAN;
    END; (* Entry *)


(* ---------------------------------------------------------------------------
 * type implementation:  KVS.Table
 * ------------------------------------------------------------------------ *)

    Table = POINTER TO TableDescriptor;

    TableDescriptor = RECORD
        lastRetrievedEntry : EntryPtr;
        entryCount,
        bucketCount        : Capacity;
        bucket             : ADDRESS (* ARRAY OF EntryPtr *)
    END; (* TableDescriptor *)


(* ---------------------------------------------------------------------------
 * function:  KVS.new ( size, status )
 * ---------------------------------------------------------------------------
 *
 * Creates  and returns a new table object  with a capacity of <size> buckets.
 * Each bucket  can hold  multiple entries.  If  zero  is passed in for <size>
 * then the table will be created  with KVS.defaultCapacity number of buckets.
 * If the value passed in for <size> is greater than KVS.maximumCapacity or if
 * memory could not be allocated then no table will be created and NIL will be
 * returned.
 *
 * The status of the operation is passed back in <status>. *)

PROCEDURE new ( size : Capacity; VAR status : Status ) : Table;

VAR
    index, bucketCount : Capacity;
    bucketPtr : EntryPtr;
    newTable : Table;

BEGIN
    
    (* bail out if size is larger than capacity limit *)
    IF size > maximumCapacity THEN
        status := invalidSize;
        RETURN NIL;
    END; (* IF *)
        
    (* determine table size *)
    IF size = 0 THEN
        bucketCount := defaultCapacity;
    ELSE
        bucketCount := size;
    END; (* IF *)
    
    (* allocate new table *)
    ALLOCATE(newTable, TSIZE(TableDescriptor) + bucketCount * TSIZE(EntryPtr));
    
    (* bail out if allocation failed *)
    IF newTable = NIL THEN
        status := allocationFailed;
        RETURN NIL;
    END; (* IF *)
    
    (* initialise table meta data *)
    newTable^.lastRetrievedEntry := NIL;
    newTable^.entryCount := 0;
    newTable^.bucketCount := bucketCount;
    
    (* initialis buckets with NIL pointers *)
    index := 0;
    WHILE index < bucketCount DO
        (* newTable^.bucket[index] := NIL; *)
        bucketPtr := ADR(newTable^.bucket) + TSIZE(EntryPtr) * index;
        bucketPtr^ := NIL;
        INC(index);
    END; (* WHILE *)

    (* pass table and status to caller *)
    status := success;
    RETURN newTable;
    
END new;


(* ---------------------------------------------------------------------------
 * function:  KVS.storeValue( table, key, val, size, zeroTerminated, status )
 * ---------------------------------------------------------------------------
 *
 * Adds a new entry  for key <key>  to table <table>.  The new entry is stored
 * by value.  Data is copied from the address passed in as <value>.  If <size>
 * is not zero, then <size> number of bytes are copied.  If <size> is zero and
 * <zeroTerminated> is true,  then data will be copied up to and including the
 * first zero-value byte encountered.  If <size> is zero  and <zeroTerminated>
 * is false,  then the operation will fail and no entry will be added.  If the
 * operation succeeds,  then the initial reference count of the new entry will
 * be set to one.  Keys must be unique.  Existing entries are not replaced.
 * Duplicate entries are not added.
 *
 * The status of the operation is passed back in <status>. *)

PROCEDURE storeValue ( table          : Table;
                       key            : Key;
                       value          : DataPtr;
                       size           : CARDINAL;
                       zeroTerminated : BOOLEAN;
                   VAR status         : Status );
VAR
    index : Capacity;
    bucket, thisEntry, newEntry : EntryPtr;

BEGIN

    (* bail out if table is NIL *)
    IF table = NIL THEN
        status := invalidTable;
        RETURN;
    END; (* IF *)
        
    (* bail out if key is zero *)
    IF key = 0 THEN
        status := invalidKey;
        RETURN;
    END; (* IF *)
    
    (* bail out if value is NIL *)
    IF value = NIL THEN
        status := invalidValue;
        RETURN;
    END; (* IF *)
    
    (* size must not be zero if data is null-terminated *)
    IF (* size unknown *) size = 0 THEN
        IF zeroTerminated THEN
            (* calculate size *)
            size := zeroTerminatedDataSize(value);
            IF size = 0 THEN
                status := invalidSize;
                RETURN;
            END; (* IF *)
        ELSE (* NOT zeroTerminated *)
            status := invalidSize;
            RETURN;
        END; (* IF *)
    END; (* IF *)
    
    (* calculate the bucket index for key *)
    index := key MOD table^.bucketCount;
    bucket := ADR(table^.bucket) + TSIZE(EntryPtr) * index;
    
    IF (* bucket is empty *) bucket^ = NIL THEN
        
        (* create a new entry *)
        newEntry := newEntryWithCopy(key, value, size, zeroTerminated, status);
        
        (* bail out if allocation failed *)
        IF newEntry = NIL THEN
            status := allocationFailed;
            RETURN;
        END; (* IF *)
        
        (* link the empty bucket to the new entry *)
        bucket^ := newEntry;
        
        (* update the entry counter *)
        INC(table^.entryCount);
        
        (* set status *)
        status := success;
        
    ELSE (* bucket is not empty *)
        
        (* check every entry in this bucket for a key match *)
        thisEntry := bucket^;
        WHILE thisEntry^.ket # key AND thisEntry^.next # NIL DO
            thisEntry := thisEntry^.next;
        END; (* WHILE *)
        
        (* the passed in key is unique if there was no key match *)
        
        IF (* key is unique *) thisEntry^.key # key THEN
            
            (* create a new entry *)
            newEntry :=
                newEntryWithCopy(key, value, size, zeroTerminated, status);
            
            (* bail out if allocation failed *)
            IF newEntry = NIL THEN
                (* status was already set *)
                RETURN;
            END; (* IF *)
            
            (* link the final entry in the chain to the new entry *)
            thisEntry^.next = newEntry;
            
            (* update the entry counter *)
            INC(table^.entryCount);
            
            (* set status *)
            status := success;
            
        ELSE (* key is not unique *)
            
            (* do not add a new entry *)
            
            (* set status *)
            status := entryNotUnique;
            
        END; (* IF *)
        
    END; (* IF *)
    
    RETURN;
    
END storeValue;


(* ---------------------------------------------------------------------------
 * function:  KVS.storeReference( table, key, val, size, zeroTerm'd, status )
 * ---------------------------------------------------------------------------
 *
 * Adds a new entry  for key <key>  to table <table>.  The new entry is stored
 * by reference.  No data is copied.  If <size> is zero  and  <zeroTerminated>
 * is true,  then  the size of the referenced data  is calculated  by counting
 * up to  and including  the  first  zero-value byte.  The size of the data is
 * then stored for faster retrieval by-copy of the entry.  Entries  stored  by
 * reference  and for which  the size  is unknown cannot be retrieved by copy.
 * The initial reference count of the new entry will be set to one.  Keys must
 * be unique.  Existing  entries  are  not  replaced.  Duplicate  entries  are
 * not added.
 *
 * The status of the operation is passed back in <status>. *)

PROCEDURE storeReference ( table          : Table;
                           key            : Key;
                           value          : DataPtr;
                           size           : CARDINAL;
                           zeroTerminated : BOOLEAN;
                       VAR status         : Status );
VAR
    index : Capacity;
    bucket, thisEntry, newEntry : EntryPtr;

BEGIN
    
    (* bail out if table is NIL *)
    IF table = NIL THEN
        status := invalidTable;
        RETURN;
    END; (* IF *)
        
    (* bail out if key is zero *)
    IF key = 0 THEN
        status := invalidKey;
        RETURN;
    END; (* IF *)
    
    (* bail out if value is NIL *)
    IF value = NIL THEN
        status := invalidValue;
        RETURN;
    END; (* IF *)
    
    (* size must not be zero if data is null-terminated *)
    IF (* size unknown *) size = 0 THEN
        IF zeroTerminated THEN
            (* calculate size *)
            size := zeroTerminatedDataSize(value);
            IF size = 0 THEN
                status := invalidSize;
                RETURN;
            END; (* IF *)
        ELSE (* NOT zeroTerminated *)
            status := invalidSize;
            RETURN;
        END; (* IF *)
    END; (* IF *)
    
    (* incomplete, to do: transcribe from C codebase *)
    
END storeReference;


(* ---------------------------------------------------------------------------
 * function:  KVS.entryExists( table, key, status )
 * ---------------------------------------------------------------------------
 *
 * Returns  TRUE  if a  valid entry  for <key>  is stored in <table>,  returns
 * FALSE otherwise.  If an entry is found,  valid or invalid,  then it will be
 * cached internally  and a  subsequent search request  for the same key  will
 * check the cached entry first,  which is  slighly faster  than a lookup of a
 * non-cached entry.  The reference count of the entry is  not  modified.
 *
 * The status of the operation is passed back in <status>. *)

PROCEDURE entryExists ( table  : Table;
                        key    : Key;
                    VAR status : Status ) : DataPtr;

BEGIN
    (* TO DO *)
END entryExists;


(* ---------------------------------------------------------------------------
 * function:  KVS.getEntry( table, copy, key, size, zeroTerminated, status )
 * ---------------------------------------------------------------------------
 *
 * Retrieves the table entry stored in <table> for <key>  either by copy or by
 * reference.  If  TRUE  is passed in for <copy>,  then  the  function returns
 * the entry's data by-copy,  otherwise it returns the data by-reference.
 *
 * For by-copy retrieval,  if the entry exists,  a newly allocated copy of its
 * value is created,  and a pointer to it is returned as function result.  The
 * size of the entry's data (in bytes)  is passed back in <size>.  However, if
 * the size of the entry's data is unknown,  then  no copy is made and  NIL is
 * returned.  The entry's reference count is never incremented when retrieving
 * by copy.
 *
 * For by-reference retrieval,  if the entry exists,  a pointer to the entry's
 * data  is returned as function result  and  the  entry's reference count  is
 * incremented.  The size of the  entry's data  (in bytes)  is  passed back in
 * <size>.  However,  if the size of the entry's data is  unknown,  then  zero
 * is passed back in <size>.
 *
 * If the entry's data is zero-terminated,  then  TRUE  will be passed back in
 * <zeroTerminated>,  otherwise  FALSE  will be passed back.
 *
 * If the entry has been successfully retrieved,  then it is cached within the
 * table,  regardless of whether it was returned by copy or by reference.
 *
 * If the entry does not exist,  or,  if it has been marked for removal,  then
 * no data is copied,  no table meta data is modified,  no entry meta data  is
 * modified  and NIL is returned.
 *
 * The status of the operation is passed back in <status>. *)

PROCEDURE getEntry ( table          : Table;
                     copy           : BOOLEAN;
                     key            : Key;
                 VAR size           : Capacity;
                 VAR zeroTerminated : BOOLEAN;
                 VAR status         : Status ) : DataPtr;

BEGIN
    (* TO DO *)
END getEntry;


(* ---------------------------------------------------------------------------
 * function:  KVS.valueForKey( table, key, status )
 * ---------------------------------------------------------------------------
 *
 * Retrieves the entry stored in table <table> for <key> and returns a pointer
 * to a  newly  allocated  copy  of the  entry's value.  The entry's reference
 * count is NOT incremented.  Entries that have been stored  by reference  and
 * are not zero-terminated can  only  be retrieved by value if their data size
 * was  explicitly  passed in  when  they  were  stored.  If  no entry  exists
 * for <key>  or if the entry is pending removal  or  if the size of the entry
 * is unknown,  then  NIL  is  returned.
 *
 * The status of the operation is passed back in <status>. *)

PROCEDURE valueForKey ( table  : Table;
                        key    : Key;
                    VAR status : Status ) : DataPtr;

BEGIN
    (* TO DO *)
END valueForKey;


(* ---------------------------------------------------------------------------
 * function:  KVS.referenceForKey( table, key, status )
 * ---------------------------------------------------------------------------
 *
 * Retrieves the entry stored in table <table> for <key> and returns a pointer
 * to the  entry's value  in the table  and  increments the  entry's reference
 * count.  If  no entry exists  for <key>  or if the entry is pending removal,
 * is unknown then NIL is returned.
 *
 * The status of the operation is passed back in <status>. *)

PROCEDURE referenceForKey ( table  : Table;
                            key    : Key;
                        VAR status : Status ) : DataPtr;

BEGIN
    (* TO DO *)
END referenceForKey;


(* ---------------------------------------------------------------------------
 * function:  KVS.sizeForKey( table, key, status )
 * ---------------------------------------------------------------------------
 *
 * Returns  the size of the data of the entry stored in <table> for <key>.  If
 * no entry exists for <key> then zero is returned.
 *
 * The status of the operation is passed back in <status>. *)

PROCEDURE sizeForKey ( table  : Table;
                       key    : Key;
                   VAR status : Status ) : CARDINAL;

BEGIN
    (* TO DO *)
END sizeForKey;


(* ---------------------------------------------------------------------------
 * function:  KVS.dataForKeyIsZeroTerminated( table, key, status )
 * ---------------------------------------------------------------------------
 *
 * Returns the zero-terminated flag of the entry stored in <table>  for <key>.
 * If no entry exists for <key> in <table> then FALSE is returned.
 *
 * The status of the operation is passed back in <status>. *)

PROCEDURE dataForKeyIsZeroTerminated ( table  : Table;
                                       key    : Key;
                                   VAR status : Status ) : BOOLEAN;

BEGIN
    (* TO DO *)
END dataForKeyIsZeroTerminated;


(* ---------------------------------------------------------------------------
 * function:  KVS.referenceCountForKey( table, key, status )
 * ---------------------------------------------------------------------------
 *
 * Returns the  reference count  of the entry stored in <table> for <key>.  If
 * no entry exists for <key> in <table> then zero is returned.  Valid  entries
 * always have a reference count greater than zero.
 *
 * The status of the operation is passed back in <status>. *)

PROCEDURE referenceCountForKey ( table  : Table;
                                 key    : Key;
                             VAR status : Status ) : CARDINAL;

BEGIN
    (* TO DO *)
END referenceCountForKey;


(* ---------------------------------------------------------------------------
 * function:  KVS.releaseEntry( table, key, status )
 * ---------------------------------------------------------------------------
 *
 * Decrements  the reference count  of the entry stored in <table>  for <key>.
 * If the entry has previously been marked for removal and its reference count
 * reaches one as a result of this release then the entry will be removed.
 *
 * The status of the operation is passed back in <status>. *)

PROCEDURE releaseEntry ( table  : Table;
                         key    : Key;
                     VAR status : Status );

BEGIN
    (* TO DO *)
END releaseEntry;


(* ---------------------------------------------------------------------------
 * function:  KVS.removeEntry( table, key, status )
 * ---------------------------------------------------------------------------
 *
 * Marks the entry stored in <table> for <key> as removed.  An entry which has
 * been marked as removed can no longer be retrieved  and will be removed when
 * its reference count reaches zero.
 *
 * The status of the operation is passed back in <status>. *)

PROCEDURE removeEntry ( table  : Table;
                        key    : Key;
                    VAR status : Status );

BEGIN
    (* TO DO *)
END removeEntry;


(* ---------------------------------------------------------------------------
 * function:  KVS.capacity( table )
 * ---------------------------------------------------------------------------
 *
 * Returns the number of buckets of <table>,  returns zero if NIL is passed in
 * for <table>. *)

PROCEDURE capacity ( table : Table ) : Capacity;

BEGIN
    (* TO DO *)
END capacity;


(* ---------------------------------------------------------------------------
 * function:  KVS.entryCount( table )
 * ---------------------------------------------------------------------------
 *
 * Returns the number of entries stored in table <table>,  returns zero if NIL
 * is passed in for <table>. *)

PROCEDURE entryCount ( table : Table ) : Capacity;

BEGIN
    (* TO DO *)
END entryCount;


(* ---------------------------------------------------------------------------
 * function:  KVS.isResizable( table )
 * ---------------------------------------------------------------------------
 *
 * Returns TRUE  if the number of buckets of <table> can change  after <table>
 * has been instatiated,  returns FALSE otherwise. *)

PROCEDURE isResizable ( table : Table ) : BOOLEAN;

BEGIN
    RETURN FALSE;
END isResizable;


(* ---------------------------------------------------------------------------
 * function:  KVS.dispose( table )
 * ---------------------------------------------------------------------------
 *
 * Disposes of table object <table> and returns NIL. *)

PROCEDURE dispose ( table : Table ) : Table;

BEGIN
    (* TO DO *)
END dispose;


END KVS.
