PRO test_nedpossearch
   rastr = '10:06:01.36'
   decstr = '-16:07:19.5'
   url = nedpos_search(rastr, decstr)
   ;
   openw, lu, 'test.html', /get_lun
   printf, lu, '<a href="'+url+'">test</a>'
   free_lun, lu
END
