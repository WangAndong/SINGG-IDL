PRO pub_singgsoft
   ;
   ; Code to export and archive singg software to web, using cvs.
   ;
   ; G. Meurer 08/2005
   pubdir = '/home/meurer/public_html/research/singg/Software/IDL/'
   dq   = '"'
   str  = systime()
   date = dq + strmid(str,8,3) + strmid(str,4,4) + strmid(str,20,4) + dq
   cmdcvs = 'cvs export -D '+date+' -d '
   ;
   cd, pubdir, current=cwd
   spawn, cmdcvs+'ImageAnalysis iman'
   spawn, cmdcvs+'Util util'
   cd, 'SINGG'
   spawn, cmdcvs+'Database singgdb'
   spawn, cmdcvs+'Filter singgfilt'
   spawn, cmdcvs+'QA singgqa'
   spawn, cmdcvs+'Sample singgsamp'
   spawn, cmdcvs+'Web singgweb'
   spawn, cmdcvs+'Proc2 proc2'
   cd, '..'
   spawn, 'chmod -R ugo+r *'
   spawn, '/bin/rm -r idlpro.tar*'
   spawn, 'tar cvf ../idlpro.tar .'
   spawn, 'mv ../idlpro.tar .'
   spawn, 'gzip -v idlpro.tar'
   ;
   cd, cwd
END 
