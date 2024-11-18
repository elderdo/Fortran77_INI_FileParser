C PARSER - parse an ini file
C     V1.0 6 AUG 03
C     Written by Douglas S. Elder elderdo@yahoo.com

C** common block that will contain the iniFile
      BLOCK DATA iniFile
      LOGICAL initialized
      INTEGER CNT
      CHARACTER LINES(100)*256
      CHARACTER iniFilename*256
      COMMON /Options/ CNT, LINES, initialized, iniFilename
      DATA initialized, CNT, iniFilename / .FALSE., 0, 'Options.ini' /
      END

C** allow you to use a different iniFile or to switch to another
      SUBROUTINE setIniFilename(value)
      CHARACTER value*(*)
      LOGICAL initialized
      INTEGER CNT
      CHARACTER LINES(100)*256
      CHARACTER iniFilename*256
      COMMON /Options/ CNT, LINES, initialized, iniFilename
      if (initialized .EQV. .TRUE.) then
        if (value .NE. iniFilename) then
      		iniFilename = value
C****** switching to a different ini file
                call loadOptions

        end if
      else
C****** overriding the default ini file
        iniFilename = value
      end if
      END

C***** search for the Section and keyword, if found return its value
C***** otherwise return the default
      SUBROUTINE getValue(section, kwd, value, default)
      IMPLICIT NONE
      CHARACTER section*(*)
      CHARACTER kwd*(*)
      CHARACTER value*(*)
      CHARACTER default*(*)
      INTEGER I
C**      WRITE(*,*) 'getValue', section, kwd, value, default
      value = ''
      call getValueX(section, kwd, value)
C**      WRITE(*,*) 'Got value ', value
      if (value .EQ. '') then
	 value = default
      end if
      END

C**** read in the iniFile into the common Options block
      SUBROUTINE loadOptions
      IMPLICIT NONE
      INTEGER CNT
      LOGICAL initialized
      CHARACTER LINES(100)*256
      CHARACTER iniFilename*256
      COMMON /Options/ CNT, LINES, initialized, iniFilename
      CHARACTER LINE*256 
C**      WRITE(*,*) 'loadOptions'
      CNT = 0
      OPEN(UNIT=33, FILE=iniFilename)
1     READ(33,'(A)', END=10) LINE
      CNT = CNT + 1
      if (CNT .GT. 100) then
C**         WRITE(0,*) 'Options.ini file > 100 lines.'
	 STOP 16
      else
	      LINES(CNT) = LINE 
      end if
      GOTO 1
10    CONTINUE
C**      WRITE(*,*) 'CNT = ',CNT
      CLOSE (UNIT=33)
      initialized = .TRUE.
      END


C*** try to find the Section and keyword, if found return its value
C*** otherwise return an empty string
      SUBROUTINE getValueX(section, kwd, value)
      IMPLICIT NONE
      CHARACTER section*(*)
      CHARACTER kwd*(*)
      CHARACTER value*(*)
      INTEGER I, J, STARTVAL
      INTEGER CNT
      LOGICAL initialized
      CHARACTER LINES(100)*256
      CHARACTER iniFilename*256
      COMMON /Options/ CNT, LINES, initialized, iniFilename
      INTEGER MAXLINE
      PARAMETER (MAXLINE = 256)
      CHARACTER LINE*256
      LOGICAL foundSection, foundKwd
      if (initialized .EQV. .FALSE.) then
            call loadOptions
      end if
      foundSection = .FALSE.
      foundKwd = .FALSE.
      value = ''
C**      WRITE(*,*) 'Looking for ',section
      DO I=1, CNT
      	if (LINES(I)(1:1) .EQ. '[') then
		DO J=2, MAXLINE
		  if (LINES(I)(J:J) .EQ. ']') then
			if (section .EQ. LINES(I)(2:J-1)) then
				foundSection = .TRUE.
C**				Write(*,*) 'foundSection=', foundSection
				GOTO 15
			end if
		  end if
		ENDDO
	else
		if (foundSection .EQV. .TRUE.) then
                        STARTVAL = 0
C**			WRITE(*,*) 'Looking for keyword ', kwd
			DO J=1, MAXLINE
				if (LINES(I)(J:J) .EQ. '=') then
					if (kwd .EQ. LINES(I)(1:J-1)) then
						STARTVAL = J+1
						foundKwd = .TRUE.
C**						Write(*,*) 'found keyword ', kwd
						GOTO 13
					end if
				end if
			END DO
13			CONTINUE
			if (foundKwd .EQV. .TRUE.) then
				DO J=MAXLINE, STARTVAL, -1
C**					Write(*,*) 'Looking for value at ', STARTVAL, J
					if (LINES(I)(J:J) .NE. '') then
						value = LINES(I)(STARTVAL:J)
C**						Write(*,*) 'Got value at line ', I, ' pos ', STARTVAL, J
						GOTO 20
					end if
				END DO
			end if
		end if
	end if
15	CONTINUE
      ENDDO
20    CONTINUE	
      END
