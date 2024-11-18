C PARSER - parse an ini file
C     V1.0 6 AUG 03
      PROGRAM MAIN
      IMPLICIT NONE
      CHARACTER exe*256
      INTEGER lastChar
      external getValue
 1000 FORMAT(A3)
 1002 FORMAT(3I2)
C
C
      call getValue('Config', 'khamExe', exe, 'default.exe')
      WRITE(*,*) 'EXE = ' // exe(1:lastChar(exe))
      call getValue('RBL', 'rblExe', exe, 'rblOn.exe')
      WRITE(*,*) 'EXE = ' // exe(1:lastChar(exe))
      call getValue('RBL', 'rblClsFile', exe, 'CLS.txt')
      WRITE(*,*) 'CLS = ' // exe(1:lastChar(exe))
      STOP
      END

