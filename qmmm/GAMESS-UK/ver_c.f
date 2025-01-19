
      subroutine ver_c_(s,lens,r,lenr,d,lend)
      character*10 r
      character*20 d
      character*70 s
      integer lens,lenr,lend
      call ver_c(s,lens,r,lenr,d,lend)
      return
      end
      subroutine ver_tsortc_(s,lens,r,lenr,d,lend)
      character*10 r
      character*20 d
      character*70 s
      integer lens,lenr,lend
      call ver_c(s,lens,r,lenr,d,lend)
      return
      end
