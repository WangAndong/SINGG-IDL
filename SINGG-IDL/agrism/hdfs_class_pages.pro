PRO hdfs_class_pages
   wd        = '/data3/acs27/meurer/grism/hdfs/axe/plots/'
   pstmp     = 'stmp_'
   pspcc     = 'spcc_'
   pspcf     = 'spcf_'
   pribn     = 'ribn_'
   pscii     = 'spec_'
   pfit      = 'spec_gfit_'
   filemcat  = 'emsource.cat'
   filemhtml = 'emsource.html'
   titleem   = 'HDFS emission line sources from grism_classify'
   filspcat  = 'spurious.cat'
   filsphtml = 'spurious.html'
   titlesp   = 'HDFS spurious interesting sources from grism_classify'
   commsp    = 'Sources marked as <i>spurious</i> with grism_classify.'
   commsp    = commsp + '  Typically these are sources on the edge of the frame, sources with likely zeroth order contamination, '
   commsp    = commsp + ' sources that are not identifiable as having emission lines or broad absorption lines, '
   commsp    = commsp + ' and other spurious sources.  Sources in both the emission line catalog and spurious catalog '
   commsp    = commsp + ' typically have a feature that is identified as a possible second (third) which is classified as spurious '
   filstcat  = 'mstar_sn.cat'
   filsthtml = 'mstar_sn.html'
   titlest   = 'HDFS broad absorption line sources from grism_classify'
   commst    = 'Sources marked as M star or SNe with grism_classify.'
   commst    = commst + '  This is a very loose classification and includes other broad absorption line sources. '
   ;
   cd, wd
   grp_emcat2html, filemcat, filemhtml, titleem, pstmp, pfit, pspcc, pspcf, pribn, pscii
   grp_logcat2html, filstcat, filsthtml, titlest, commst, pstmp, pspcc, pspcf, pribn, pscii
   grp_logcat2html, filspcat, filsphtml, titlesp, commsp, pstmp, pspcc, pspcf, pribn, pscii
   ;
END 
