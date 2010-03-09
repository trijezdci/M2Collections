(* Modula-2 Collections Library
 *
 *  @file Hash.mod
 *  Hash implementation
 *
 *  General purpose hash algorithm
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


IMPLEMENTATION (* OF *) MODULE Hash;

FROM SYSTEM IMPORT CAST, BITSET32, SHIFT;


CONST

(* ---------------------------------------------------------------------------
 * bit mask constant to apply to final hash value
 * ------------------------------------------------------------------------ *)

    finalMask = 07FFFFFFFH;

VAR

(* ---------------------------------------------------------------------------
 * intermediate variable for bit mask type cast
 * ------------------------------------------------------------------------ *)

    finalMaskValue : Key;


(* ---------------------------------------------------------------------------
 * function:  Hash.valueForNextChar( hash, ch )
 * ------------------------------------------------------------------------ *)

PROCEDURE valueForNextChar ( hash : Key; ch : CHAR ) : Key;

BEGIN
    (* RETURN ORD(ch) + SHL(hash, 6) + SHL(hash, 16) - hash; *)
    RETURN CAST(Key, ORD(ch)) +
           CAST(Key, SHIFT(CAST(BITSET32, (hash)), 6)) +
           CAST(key, SHIFT(CAST(BITSET32, (hash)), 16)) - hash;
END valueForNextChar;


(* ---------------------------------------------------------------------------
 * function:  Hash.finalValue( hash )
 * ------------------------------------------------------------------------ *)

PROCEDURE finalValue ( hash : Key ) : Key;

BEGIN
    (* RETURN BWAND(hash, finalMask); *)
    RETURN CAST(Key, CAST(BITSET32, hash) * CAST(BITSET32, finalMaskValue));
END finalValue;


(* ---------------------------------------------------------------------------
 * function:  Hash.valueForString( str )
 * ------------------------------------------------------------------------ *)

PROCEDURE valueForString ( str : ARRAY OF CHAR ) : Key;

VAR
    ch    : CHAR;
    hash  : Key;
    index : CARDINAL;
    
BEGIN
    index := 0;
    hash := initialValue;
    
    ch := str[index];
    WHILE ch # 0C AND index < HIGH(str) DO
        (* hash := ORD(ch) + SHL(hash, 6) + SHL(hash, 16) - hash; *)
        hash := CAST(key, ORD(ch)) +
                CAST(Key, SHIFT(CAST(BITSET32, (hash)), 6)) +
                CAST(key, SHIFT(CAST(BITSET32, (hash)), 16)) - hash;
        INC(index);
        ch := str[index];
    END; (* WHILE *)
    
    (* RETURN BWAND(hash, finalMask); *)
    RETURN CAST(Key, CAST(BITSET32, hash) * CAST(BITSET32, finalMaskValue));
END valueForString;


(* ---------------------------------------------------------------------------
 * module initialisation
 * ------------------------------------------------------------------------ *)

BEGIN
    (* This nonsense wouldn't be necessary if CAST allowed constants *)
    finalMaskValue := finalMask;
END Hash.
