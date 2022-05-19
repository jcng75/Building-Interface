; Program template (Template.asm)

COMMENT&
The great project...
NOTE:  Please do not make any breakpoints for simply testing cases!  I have written the program so
that breakpoints are NOT required.
&

; Either include the Irvine libary
INCLUDE Irvine32.inc

; or include the .386 and .model directives
.386
.model flat, stdcall

.stack 4096; declare your stack size

; Declare the function prototype for the ExitProcess function
ExitProcess PROTO dwExitCode : DWORD

addBuilding PROTO buildingName: PTR BYTE, unitCount: DWORD
removeBuilding PROTO buildingName: PTR BYTE
leaseUnit PROTO buildingName: PTR BYTE, tenantName: PTR BYTE, rentCost:REAL8
unleaseUnit PROTO buildingName: PTR BYTE, inputTenantName: PTR BYTE
rentRoll PROTO buildingName: PTR BYTE
occupancyReport PROTO
checkUnits PROTO buildingName: PTR BYTE
printBuildings PROTO
printUnits PROTO buildingName: PTR BYTE

Building STRUCT
	buildName BYTE 50 DUP (0), 0
	align DWORD
	numUnits DWORD 0
	unitsPtr DWORD 0
	currOccupancy DWORD 0
	nextPtr DWORD 0
Building ENDS

unit STRUCT
	occupied BYTE 0
	tenantName BYTE 50 DUP(0), 0
	align DWORD
	rent REAL8 0.00
unit ENDS

.data
; declare variables here
MAX = 50
buildingNameUse BYTE MAX+1 DUP(?)
unitName BYTE MAX+1 DUP(?)

magicNumber REAL8 100.00

hHeap DWORD ?		; heap pointer
buildingsArray DWORD ?
totalRent REAL8 0.00
tempHolder REAL8 0.00
occPercentage REAL8 0.00
totalOcc DWORD 0
totalUnits DWORD 0
totalOccPercentage REAL8 0.0
percentage DWORD ?
rentRound DWORD ?
dummyVal DWORD ?
dummyPtr1 DWORD 50 DUP(0), 0
dummyPtr2 DWORD 50 DUP(0), 0
percentChar DWORD "%", 0Dh, 0Ah
dollarChar DWORD "$", 0Dh, 0Ah
cleanValue REAL8 0.00			; Clear totalRent

menu1 BYTE "1) Create Building", 0Dh, 0Ah
	   BYTE "2) Delete Building", 0Dh, 0Ah
	   BYTE "3) Lease Unit", 0Dh, 0Ah
	   BYTE "4) Unlease Unit", 0Dh, 0Ah
	   BYTE "5) Rent Roll", 0Dh, 0Ah
	   BYTE "6) Occupancy Report", 0Dh, 0Ah
	   BYTE "7) EXIT", 0Dh, 0Ah, 0

prompt1 BYTE "Please choose an option:", 0Dh, 0Ah, 0
promptKey BYTE "Please enter any key to continue...", 0Dh, 0Ah, 0

buildingPrompt1 BYTE "Please specify the name of the building (LIMIT 50 CHARACTERS):", 0Dh, 0Ah, 0
buildingPrompt2 BYTE "Please specify the maximum number of units in your building: ", 0Dh, 0Ah, 0

buildingCreationSuccess BYTE "Building has been sucesfully added!", 0Dh, 0Ah, 0
buildingRemovalSuccess BYTE "Building has been sucesfully removed!", 0Dh, 0Ah, 0

errorMenuMsg BYTE "Invalid Input! Please enter value from 1-7!", 0Dh, 0Ah, 0			; Main Menu Error Msg
errorCreateMsg1 BYTE "Invalid Input! That building name is already in use!", 0Dh, 0Ah, 0	; Building Creation Error Msg
errorCreateMsg2 BYTE "Invalid Input! Please specify a valid number!", 0Dh, 0Ah, 0		; Building Creation Error Msg 2
errorMissingMsg BYTE "Invalid Input! That building doesn't exist!", 0Dh, 0Ah, 0			; Building Destruction Error Msg 
errorMissingMsg2 BYTE "Error!  You cannot do the following action, as no buildings exist!", 0Dh, 0Ah, 0		; Error for lease/unlease/rentroll

unitPrompt1 BYTE "Please specify the name of the tenant in the unit (LIMIT 50 CHARACTERS):", 0Dh, 0Ah, 0
unitPrompt2 BYTE "Please specify the cost of rent for this unit:", 0Dh, 0Ah, 0

rentRollMsg1 BYTE "Currently occupied tenants: ", 0Dh, 0Ah, 0
rentRollMsg2 BYTE "Total rent of the building: ", 0Dh, 0Ah, 0
rentRollMsg3 BYTE "Occupancy percentage of building: ", 0Dh, 0Ah, 0

occupancyMsg1 BYTE "Building name: ", 0Dh, 0Ah, 0
occupancyMsg2 BYTE "Current occupancy percentage: ", 0Dh, 0Ah, 0
occupancyMsg3 BYTE "Total occupancy percentage for all buildings:", 0Dh, 0Ah, 0

printBuildingPrompt BYTE "All available buildings: ", 0Dh, 0Ah, 0

unitLeaseSuccess BYTE "Unit has been succesfully leased!", 0Dh, 0Ah, 0
unitUnleaseSuccess BYTE "Unit has been succesfully unleased!", 0Dh, 0Ah, 0

errorCreateUnitMsg1 BYTE "Invalid Input!  The rent ammount must be above $0.00...", 0Dh, 0Ah, 0
checkUnitsError BYTE "ERROR! Cannot add any more units, building is full!", 0Dh, 0Ah, 0

unitUnleaseError BYTE "Invalid Input!  That Tenant Name doesn't exist!", 0Dh, 0Ah, 0
errorUnleaseMsg BYTE "ERROR! There are no avilable units to unlease in this building...", 0Dh, 0Ah, 0

endMsg BYTE "Terminating Process...", 0


.code
main PROC
; write your code here
; create heap to use
INVOKE GetProcessHeap
; if heap succeeds, move the value to the handle variable
.IF eax == NULL
jmp quit
.ELSE
mov hHeap, eax
.ENDIF
mainMenu:
	; in the main menu, we will get a character from the user
	; if the input is between 1-7, we will proceed to the next code label
	call clrscr
	call Crlf
	mov EDX, offset menu1
	call WriteString
	mov EDX, offset prompt1
	call WriteString
	call getNextChar
	cmp al, '1'
	je option1
	cmp al, '2'
	je option2
	cmp al, '3'
	je option3
	cmp al, '4'
	je option4
	cmp al, '5'
	je option5
	cmp al, '6'
	je option6
	cmp al, '7'
	je quit
	call Crlf
	mov edx, offset errorMenuMsg
	call WriteString
	call Crlf
	call pressContinue
	jmp mainMenu

option1:
	; Option 1 is for creating the building
	; We first ask for user input for the name of the building
	call clrscr
	call Crlf
	mov edx, offset buildingPrompt1
	call WriteString
	mov edx, MAX
	mov esi, offset buildingNameUse
	call getNextString
	; save that name to ebx register
	mov ebx, edx
nextPrompt1:
	; After getting their name, we will get the number of units that the building contains
	mov edx, offset buildingPrompt2
	call WriteString
	call ReadInt ; check this with readint
	; If input invalid we goto error handling
	jo invalidInput1
	cmp eax, 0
	je invalidInput1

	; create the building
	INVOKE addBuilding, ebx, eax
	; Print success statement and go back to mainmenu
	mov edx, offset buildingCreationSuccess
	call WriteString
	; Clean the variables used for input
	mov esi, offset buildingNameUse
	mov ecx, lengthof buildingNameUse
	dec ecx
	call clearString
	call pressContinue
	jmp mainMenu
invalidInput1:
	; Display error message and request proper input again
	mov esi, offset errorCreateMsg2
	call displayErrorMsg
	jmp nextPrompt1
option2:
	; Option 2 is for the removal of a building
	; We first attempt to display all buildings available
	; If there are none, eax would be 0 and we would print out an error message
	call clrscr
	call Crlf
	INVOKE printBuildings
	.IF eax == 0
		mov edx, offset errorMissingMsg2
		call WriteString
		call pressContinue
		jmp mainMenu
	.ENDIF
	; Otherwise we then again ask for input again for the building name
	mov edx, offset buildingPrompt1
	call WriteString
	mov edx, MAX
	mov esi, offset buildingNameUse
	call getNextString
	call padString
	; Invoke the procedure, if it succeeds jump to success code label, otherwise jump to error
	; The only error possible is if the building doesn't exist at this point
	INVOKE removeBuilding, edx
	cmp eax, 1
	je removalSuccess
	jmp removalError
	
	removalSuccess:
	mov edx, offset buildingRemovalSuccess
	call WriteString
	jmp quitRemoval
	removalError:
	mov edx, offset errorMissingMsg
	call WriteString
	; Regardless, make sure to clean variable input
	quitRemoval:
	mov esi, offset buildingNameUse
	mov ecx, lengthof buildingNameUse
	dec ecx
	call clearString
	call pressContinue
	jmp mainMenu

option3:
	; Same logic as 2nd option, but with leasing a building
	; Check if there are any buildings, if not, print error
	; Otherwise list out buildings first
	call clrscr
	call Crlf
	INVOKE printBuildings
	.IF eax == 0
		mov edx, offset errorMissingMsg2
		call WriteString
		call Crlf
		call pressContinue
		jmp mainMenu
	.ENDIF
	; Get user input for building again
	mov edx, offset buildingPrompt1
	call WriteString
	mov edx, MAX
	mov esi, offset buildingNameUse
	call getNextString
	call padString
	; Check if this building exists and if the building has available space
	; If not, display error message
	INVOKE checkUnits, edx
	.IF eax == 0
		call pressContinue
		jmp mainMenu
	.ENDIF
	; Save inputted building name pointer to ebx register
	; Call moveString to save value onto dummyPtr1 var
	mov ebx, edx
	mov edi, offset dummyPtr1
	call moveString
	.IF eax == 1
	; Possible to lease a unit, now make a unitName and get rent
		getUnitName:
		mov edx, offset unitPrompt1
		; Get unit name
		call WriteString
		mov edx, MAX
		mov esi, offset unitName
		call getNextString
		call padString
		; Save inputted building name pointer to ebx register
		; Call moveString to save value onto dummyPtr2 var
		mov ebx, edx
		mov edi, offset dummyPtr2
		call moveString
		; in the next block, we get the rent of the building
		; check if building has invalid input, otherwise proceed
		getRent:
			mov edx, offset unitPrompt2		; Enter rent amount
			call WriteString
			call ReadDec
			jnc succeed
			mov edx, offset errorCreateUnitMsg1		; Invalid input
			call WriteString
			jmp getRent
	.ELSE
		call pressContinue
		jmp mainMenu
	.ENDIF
	succeed:
	; Load the inputted value into a real8 variable
	mov esi, offset dummyVal
	mov [esi], eax
	FILD dummyVal
	FSTP tempHolder
	; Invoke function call
	INVOKE leaseUnit, ADDR dummyPtr1, ADDR dummyPtr2, tempHolder
	; Succeeding we print succes, otherwise we print an error
	.IF eax == 1
		mov edx, offset unitLeaseSuccess
		call WriteString
	.ENDIF
	; Clear all strings used for the procedure
	mov esi, offset buildingNameUse
	mov ecx, lengthof buildingNameUse
	dec ecx
	call clearString
	mov esi, offset unitName
	mov ecx, lengthof unitName
	dec ecx
	call clearString
	mov esi, offset dummyPtr1
	mov ecx, lengthof dummyPtr1
	dec ecx
	call clearString
	mov esi, offset dummyPtr2
	mov ecx, lengthof dummyPtr2
	dec ecx
	call clearString
	endLease:
	call pressContinue
	jmp mainMenu
option4:
	; Option 4 is for unleasing a unit
	; Again, first print buildings if possible, otherwise error
	call clrscr
	call Crlf
	INVOKE printBuildings
		.IF eax == 0
		mov edx, offset errorMissingMsg2
		call WriteString
		call pressContinue
		jmp mainMenu
	.ENDIF
	; Get the building name as input 
	mov edx, offset buildingPrompt1
	call WriteString
	mov edx, MAX
	mov esi, offset buildingNameUse
	call getNextString
	; Move that variable into dummyPtr1
	mov ebx, edx
	mov edi, offset dummyPtr1
	call moveString
	; Now we print LEASED units for the building, if no units are leased, we return an error
	INVOKE printUnits, ADDR buildingNameUse
	.IF eax == 0
		mov edx, offset errorUnleaseMsg
		call WriteString
		call Crlf
		call pressContinue
		jmp mainMenu
	.ENDIF
	unitInput3:
	; get the tenant name we are looking to unlease
	mov edx, offset unitPrompt1
		call WriteString
		mov edx, MAX
		mov esi, offset unitName
		call getNextString
		call padString
	mov ebx, edx
	mov edi, offset dummyPtr2
	call moveString
	; Invoke the call, if succeeds we return success
	; Otherwise we return error
	INVOKE unleaseUnit, ADDR dummyPtr1, ADDR dummyPtr2
	.IF eax == 1
		mov edx, offset unitUnleaseSuccess
		call WriteString
	.ELSE
		mov edx, offset unitUnleaseError		; Unit name doesnt exist
		call WriteString
	.ENDIF
	; Clear all string variables used for procedure
	mov esi, offset unitName
	mov ecx, lengthof unitName
	dec ecx
	call clearString
	mov esi, offset buildingNameUse
	mov ecx, lengthof buildingNameUse
	dec ecx
	call clearString
	call Crlf
	call pressContinue
	jmp mainMenu
option5:
	; Option 5 is the rent roll procedure
	; Check if there are any available buildings, if so print them out
	call clrscr
	call Crlf
	INVOKE printBuildings
	.IF eax == 0
		mov edx, offset errorMissingMsg2
		call WriteString
		call pressContinue
		jmp mainMenu
	.ENDIF
	; Get the user's input for a building to view
	mov edx, offset buildingPrompt1
	call WriteString
	mov edx, MAX
	mov esi, offset buildingNameUse
	call getNextString
	; Invoke the call and then clean the variable afterwards
	INVOKE rentRoll, edx
	mov esi, offset buildingNameUse
	mov ecx, lengthof buildingNameUse
	dec ecx
	call clearString
	call Crlf
	call pressContinue
	jmp mainMenu
option6:
	; Option 6 is the occupancy report
	call clrscr
	call Crlf
	; Just invoke proc
	INVOKE occupancyReport
	call pressContinue
	jmp mainMenu
quit:
	; Last call is quit
	; Simple as it sounds, clear the screen and print out terminating string
	call clrscr
	mov edx, offset endMsg
	call WriteString
	call Crlf
	call pressContinue
exit
main ENDP

addBuilding PROC USES eax, buildingName: PTR BYTE, unitCount:DWORD


INVOKE HeapAlloc, hHeap, HEAP_ZERO_MEMORY, SIZEOF Building

.IF eax == NULL
	; Allocation fails
	mov eax, 0
	stc
.ELSE
	
	; Store ptr of building before overriding value
	mov ebx, eax

	; Allocation suceeds	
	mov esi, buildingName
	lea edi, (building PTR [eax]).buildName
	mov ecx, 51
	; call Str_Length ; ecx == len(building)

	; Copy the buildingName given into the building struct parameter
	copyLoop:
	mov dl, [esi]
	mov [edi], dl
	inc esi
	inc edi
	loop copyLoop

	; Calculate the amount of space it will take to create units space
	mov eax, SIZEOF unit
	mul unitCount

	; Allocate the units for the building on heap
	; After doing so, move that handle into building's unitPtr field
	INVOKE HeapAlloc, hHeap, HEAP_ZERO_MEMORY, eax
	mov (building PTR [ebx]).unitsPtr, eax
	
	mov edx, unitCount
	mov (building PTR [ebx]).numUnits, edx

	.ENDIF

	; We must add our building into the buildingArray (pointer)
	; First check if the pointer is pointing to nothing
	.IF buildingsArray == NULL
		; If so, we simply make the pointer point to just the new building
		mov buildingsArray, ebx

	.ELSE
		; If a building exists, get the original pointer for building
		; Continue to check each building's following building pointer until you find a null
		; If the building's next pointer is null, you know that you've reached the end of the list
		; From there, add our newest building to the end of the list
		mov eax, buildingsArray
		pointersLoop:
		mov edx, (building PTR [eax]).nextPtr
		.IF edx == NULL
			mov (building PTR [eax]).nextPtr, ebx
		.ELSE
			mov eax, (building PTR [eax]).nextPtr
			jmp pointersLoop
		.ENDIF
	.ENDIF
	clc
	mov eax, 1
ret
addBuilding ENDP

removeBuilding PROC USES edi edx esi, buildingName: PTR BYTE
mov esi, buildingsArray					; back pointer
mov edx, esi							; second back pointer
cmp esi, 0	; check if array is empty
je error
findBuilding:
	lea edi, (building PTR [edx]).buildName
	; If we found the right building, jump to next coding block
	; Otherwise, update the registers to search for the next building
	INVOKE Str_compare, edi, buildingName
	je matchFound
	mov edi, (building PTR [edx]).nextPtr		; update edi to current nextPtr
	.IF edi == NULL	; Did we reach the end of the arrays list?
		jmp error		; No find
	.ELSE
		mov esi, edx	; update esi to be used for next loop
		mov edx, edi	; update edx 
		jmp findBuilding	; Found
	.ENDIF
matchFound:
	; If esi == buildingsArray, we must change the buildingsArray pointer itself
	.IF esi == buildingsArray
		lea edi, (building PTR [esi]).nextPtr
		mov eax, [edi]
		mov esi, offset buildingsArray
		mov [esi], eax
		mov eax, 1
		jmp quit
	.ENDIF
	; Otherwise, we can simply move over the middle pointer within the three buildings
	; looks like this [esi]-[edx]-[eax] 
	lea edi, (building PTR [edx]).nextPtr
	mov eax, [edi]
	mov (building PTR [esi]).nextPtr, eax	; Set prev val's nextPtr to the nextPtr val of current building
	; If succeeds, move eax to 1, otherwise 0
	mov eax, 1
	jmp quit
error:
	mov eax, 0
quit:
ret
removeBuilding ENDP

checkUnits PROC, buildingName: PTR BYTE
; check if buildings array is empty, if so error handle
mov esi, buildingsArray
cmp esi, 0
je buildingNotFound
findBuilding:
; Look for building similar way to removing building
lea edi, ((building PTR [esi]).buildName)	
	INVOKE Str_compare, buildingName, edi
	je foundBuilding
	mov esi, (building PTR [esi]).nextPtr
	; If we searched whole linked list, we error handle
	.IF esi == NULL
		jmp buildingNotFound
	.ENDIF
	jmp findBuilding
buildingNotFound:
	; Error handle for if building doesn't exist
	mov edx, offset errorMissingMsg
	call WriteString
	call Crlf
	mov eax, 0
	ret
foundBuilding:
	; After finding the building, check if the building has available units to lease
	; If not, we jump to tooManyUnits and return 0 to eax
	; Otherwise return 1
	mov eax, (building PTR [esi]).numUnits
	mov ebx, (building PTR [esi]).currOccupancy
	.IF eax == ebx
		jmp tooManyUnits
	.ENDIF
	mov eax, 1
	ret
tooManyUnits:
	mov eax, 0
	mov edx, offset checkUnitsError
	call WriteString
	ret
checkUnits ENDP

leaseUnit PROC USES edi esi edx, buildingName: PTR BYTE, inputTenantName: PTR BYTE, rentCost:REAL8 
; Look for building similar way to removing building
mov esi, buildingsArray
cmp esi, 0
je buildingNotFound
findBuilding:
	; Same way as past 2 procedures above
	lea edi, ((building PTR [esi]).buildName)		
	INVOKE Str_compare, edi, buildingName
	je foundBuilding
	mov esi, (building PTR [esi]).nextPtr
	.IF esi == NULL
		jmp buildingNotFound
	.ENDIF
	jmp findBuilding
buildingNotFound:
	mov eax, 0
	ret
foundBuilding:
	; Check if possible to lease units
	mov eax, (building PTR [esi]).numUnits
	mov ebx, (building PTR [esi]).currOccupancy
	.IF eax == ebx
		jmp tooManyUnits
	.ELSE
		; Increase the total occupancy of building
		; Get the units pointer and find an available unit
		; Once that is done, give the inputted paramters to their respective field
		inc (building PTR [esi]).currOccupancy
		mov edi, (building PTR [esi]).unitsPtr
		findAvailUnit:
		mov al, (unit PTR [edi]).occupied
			.IF al == 0		; "Is it available"
			mov (unit PTR [edi]).occupied, 1
			FLD rentCost
			FSTP (unit PTR [edi]).rent
			mov esi, inputTenantName
			mov edx, esi
			mov ecx, 51
			lea edi, (unit PTR[edi]).tenantName
			copyLoop:
				mov dl, [esi]
				mov [edi], dl
				inc esi
				inc edi
			loop copyLoop
			mov eax, 1
			ret
			.ELSE
			add edi, SIZEOF unit
			jmp findAvailUnit
			.ENDIF
	.ENDIF
tooManyUnits:
	mov eax, 0
	ret
leaseUnit ENDP

unleaseUnit PROC USES ebx edx edi esi, buildingName: PTR BYTE, inputTenantName: PTR BYTE
; Look for building similar way to removing/unleasing building/unit
mov esi, buildingsArray
findBuilding:
	lea edi, ((building PTR [esi]).buildName)		; Not working?
	INVOKE Str_compare, buildingName, edi
	je foundBuilding
	mov esi, (building PTR [esi]).nextPtr
	.IF esi == NULL
		jmp buildingNotFound
	.ENDIF
	jmp findBuilding
buildingNotFound:
	mov eax, 0
	ret
foundBuilding:
	; Logic finding unit is identical to finding building
	; Just get unitPtr instead of buildingPtr
	; Use unit ptr to get tenantName and compare it with inputTenantName
	mov ebx, 0
	mov eax, esi
	mov edi, ((building PTR [esi])).unitsPtr
	mov edx, ((building PTR [esi])).numUnits
	findUnit:
	lea esi, (unit PTR [edi]).tenantName
	INVOKE Str_compare, esi, inputTenantName
	je foundUnit
	inc ebx
	; If we searched through all the possible units and still haven't found the unit, return 0 to eax
	.IF ebx == edx	; "There are no more units to check"
		mov eax, 0
		ret
	.ENDIF
	; Each time we increment the pointer by the size of each unit
	add edi, SIZEOF unit
	jmp findUnit
	foundUnit:
	; If we find the unit, change the occupancy to 0
	; and clear the name within the unit
	; Also decrement current occupancy for building
	dec (building PTR [eax]).currOccupancy
	mov (unit PTR [edi]).occupied, 0
	add edi, SIZEOF unit
	mov ecx, 50
	call ClearString
	mov eax, 1
	ret
unleaseUnit ENDP

rentRoll PROC, buildingName: PTR BYTE
; Find building same way as previous procedures
call Crlf
mov esi, buildingsArray
findBuilding:
	lea edi, ((building PTR [esi]).buildName)
	INVOKE Str_compare, buildingName, edi
	je foundBuilding
	mov esi, (building PTR [esi]).nextPtr
	.IF esi == NULL
		jmp buildingNotFound
	.ENDIF
	jmp findBuilding
foundBuilding:
	; Within this part, we print out the following:
	; List of units within the building
	; # of units, occupancy percentage, total rent
	mov edx, offset occupancyMsg1		; Write building prompt EX: "Building name: "
	call WriteString
	mov edx, edi		; Move buildname offset into edx
	call WriteString	; Write Building
	call Crlf
	mov ebx, esi		; edx holds onto the esi pointer
	mov esi, (building PTR [ebx]).unitsPtr		; Get the array of units
	mov edx, offset rentRollMsg1		; Write unit prompt EX: "Currently used units: "
	call WriteString
	mov ecx, (building PTR [ebx]).numUnits
	unitsLoop:
		; In the units loop check if unit is occupied,
		; If so, we print out the tenant name
		; And add it's rent to the total 
		lea edi, (unit PTR [esi]).occupied
		mov eax, [edi]
		cmp eax, 0
		je skip
		lea edx, (unit PTR [esi]).tenantName
		call WriteString
		call Crlf
		FLD (unit PTR [esi]).rent
		FLD totalRent
		FADD ST(0), ST(1)
		FSTP totalRent
		FSTP tempHolder
		skip:
		add esi, sizeof unit
		loop unitsLoop
	; Print out total rent of building
	mov edx, offset rentRollMsg2			; Write unit prompt EX: "Total rent for building: "
	call WriteString
	fld totalRent
	FRNDINT
	FISTP rentRound
	mov eax, rentRound
	mov edx, offset dollarChar
	call WriteString
	call WriteDec
	call Crlf
	FSTP totalRent

	; calc occ percentage
	FILD (building PTR [ebx]).numUnits
	FILD (building PTR [ebx]).currOccupancy
	FDIV ST(0), ST(1)

	FSTP occPercentage
	FSTP tempHolder
	mov edx, offset rentRollMsg3			; Write unit prompt EX: "Current occupancy percentage: "
	call WriteString
	fld magicNumber
	fld occPercentage
	FMUL ST(0), ST(1)
	FRNDINT
	FISTP percentage
	FSTP magicNumber
	mov eax, percentage
	call WriteDec
	mov edx, offset percentChar
	call WriteString
	call Crlf
	FLD cleanValue
	FSTP totalRent

buildingNotFound:
mov eax, 0
error:
ret
rentRoll ENDP

occupancyReport PROC
call Crlf
mov esi, buildingsArray
cmp esi, 0
je error
buildingLoop:
	; Iterate through all buildings
	; Within each building, calculate their current occupancy percentage and print it
	; and add both their number of units and current occupency to 
	; the variables occPercentage and totalUnits
	mov edx, offset occupancyMsg1	; Building Name:
	call WriteString
	call Crlf
	lea edi, ((building PTR [esi]).buildName)
	mov edx, edi
	call WriteString
	call Crlf
	FILD ((building PTR [esi]).numUnits)
	FILD ((building PTR [esi]).currOccupancy)
	FDIV ST(0), ST(1)
	FSTP occPercentage
	FSTP tempHolder
	FLD magicNumber
	FLD occPercentage
	FMUL ST(0), ST(1)
	FRNDINT			; check later
	FISTP percentage
	FSTP magicNumber
	mov eax, percentage
	mov edx, offset occupancyMsg2			; Current Building's occupancy:
	call WriteString
	mov edx, offset percentChar			; Add %
	call WriteDec
	call WriteString
	call Crlf
	mov eax, (building PTR [esi]).currOccupancy		
	add eax, totalOcc
	mov totalOcc, eax
	mov eax, (building PTR [esi]).numUnits
	add eax, totalUnits
	mov totalUnits, eax

	; Once we reach the end, move onto more calculations
	mov esi, (building PTR [esi]).nextPtr
	.IF esi != NULL
		jmp buildingLoop

	.ENDIF

	; Calculate total occupancy percentage
	; Round floating point number to nearest whole number and multiplying by 100
	FILD totalUnits
	FILD totalOcc
	FDIV ST(0), ST(1)
	FSTP totalOccPercentage
	FSTP tempHolder
	FLD magicNumber
	FLD totalOccPercentage
	FMUL ST(0), ST(1)
	FRNDINT
	FISTP percentage
	FSTP magicNumber
	mov eax, percentage
	; Print occupancy
	mov edx, offset occupancyMsg3		; Total Occupancy percentage
	call WriteString
	call WriteDec
	mov edx, offset percentChar			; Add %
	call WriteString
	call Crlf
	jmp quit
error:
	mov edx, offset errorMissingMsg2
	call WriteString
quit:
ret
occupancyReport ENDP

; This is a helper function to print out all buildings
; It will also exception check if there are no buildings available
printBuildings PROC USES esi edi edx
mov esi, buildingsArray
cmp esi, 0
je noBuildings
mov edx, offset printBuildingPrompt
call WriteString
buildingTraversal:
	lea edi, ((building PTR [esi]).buildName)
	mov edx, edi
	call WriteString
	call Crlf
	mov esi, (building PTR [esi]).nextPtr
	.IF esi == NULL
		jmp quit
	.ENDIF
	jmp buildingTraversal
	quit:
	mov eax, 1
	ret
noBuildings:
	mov eax, 0
	ret
printBuildings ENDP

; This is a helper function to print out all units of a building
; This does NOT exception check for if any units exist as it is checked elsewhere
printUnits PROC buildingName: PTR BYTE
mov esi, buildingsArray
findBuilding:
	lea edi, ((building PTR [esi]).buildName)
	INVOKE Str_compare, buildingName, edi
	je foundBuilding
	mov esi, (building PTR [esi]).nextPtr
	.IF esi == NULL
		jmp buildingNotFound
	.ENDIF
	jmp findBuilding
foundBuilding:
	mov ebx, esi		; edx holds onto the esi pointer
	mov esi, (building PTR [ebx]).currOccupancy
	.IF esi == 0
		mov eax, 0
		ret
	.ENDIF
	mov esi, (building PTR [ebx]).unitsPtr		; Get the array of units
	mov edx, offset rentRollMsg1		; Write unit prompt EX: "Currently used units: "
	call WriteString
	mov ecx, (building PTR [ebx]).numUnits
	unitsLoop:
		lea edi, (unit PTR [esi]).occupied
		cmp edi, 0
		je skip
		lea edx, (unit PTR [esi]).tenantName
		call WriteString
		call Crlf
		skip:
		add esi, sizeof unit
		loop unitsLoop
	ret
	buildingNotFound:
	mov eax, 0
	ret
printUnits ENDP

; This function allows us to get the next character from the keyboard
getNextChar PROC
	 call ReadChar		; input from keyboard
	 call WriteChar		; echo on screen
	 ret
getNextChar ENDP

; This function allows us to get the next string from the keyboard after the user inputs enter
getNextString PROC
	mov ecx, edx
	mov edx, esi
	call ReadString
	ret
getNextString ENDP

; This function can be used to display error messages (although i didn't use it that much...)
displayErrorMsg PROC
	mov edx, esi
	call WriteString
	ret
displayErrorMsg ENDP

; This function allows the user to clear strings for variables that will be reused
clearString PROC USES EAX
mov al, 0
L1:
	mov [esi], al
	inc esi
	Loop L1
ret

clearString ENDP

; This function is used to pad the string if the user didn't use the maximum amount of characters
; In our case, we set the max to 50.  If the user inputs "test", this procedure will pad the string to
; be "test0000000..." so it uses up the whole building name slot
padString PROC USES EAX
call StrLength
add esi, eax
mov ebx, 50
sub ebx, eax
mov ecx, ebx
mov al, 0
L1:
	mov [esi], al
	loop L1
ret
padString ENDP

; This function was a helper function that was used for moving strings into variables
; This was used for things like moving strings into dummyPtr1 and dummyPtr2
moveString PROC USES esi edi eax edx 
mov esi, ebx
mov ecx, 50
L1:
	mov al, [esi]
	mov [edi], al
	inc esi
	inc edi
	loop L1
ret
moveString ENDP

; This helper function was a bit of style, as it allows causes the user of the interface to enter something to continue
; Again, please don't make any breakpoints, they're not needed :)
pressContinue PROC USES edx
	mov EDX, offset promptKey
	call WriteString
	call getNextChar
	ret
pressContinue ENDP

END main