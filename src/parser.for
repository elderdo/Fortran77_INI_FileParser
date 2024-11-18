C PARSER - parse an ini file
C     V1.0 6 AUG 03
      PROGRAM MAIN
      IMPLICIT NONE

 1000 FORMAT(A3)
 1002 FORMAT(3I2)
C
      INTEGER CNT
      INTEGER I, J, STARTVAL
      INTEGER MAXLINE
      PARAMETER (MAXLINE = 80)
      CHARACTER LINE*80
      CHARACTER LINES(100)*80
      LOGICAL foundSection
      CNT = 0
      foundSection = .FALSE.
1     READ(*,'(A)', END=10) LINE
      CNT = CNT + 1
      LINES(CNT) = LINE 
      WRITE(*,*) LINE
      GOTO 1
10    WRITE(*,*) CNT
      DO I=1, CNT
      	if (LINES(I)(1:1) .EQ. '[') then
		WRITE(*,*) 'Start Section'
		DO J=2, MAXLINE
		  if (LINES(I)(J:J) .EQ. ']') then
			WRITE(*,*) 'Name is ' // LINES(I)(2:J-1)
			foundSection = .TRUE.
			GOTO 15
		  end if
		ENDDO
	else
		if (foundSection .EQV. .TRUE.) then
			DO J=1, MAXLINE
				if (LINES(I)(J:J) .EQ. '=') then
					WRITE(*,*) 'KWD = ' // LINES(I)(1:J-1)
					STARTVAL = J+1
					GOTO 13
				end if
			END DO
13			CONTINUE
			DO J=MAXLINE, STARTVAL, -1
				if (LINES(I)(J:J) .NE. '') then
					WRITE(*,*) 'VALUE = ' // LINES(I)(STARTVAL:J)
					GOTO 14
				end if
			END DO
14			CONTINUE
		end if
	end if
15	CONTINUE
      ENDDO
C
      STOP
      END
      BLOCK DATA OPTIONSINI
      COMMON /INIFILE/ CHARACTER LINES(100)*80
      END
