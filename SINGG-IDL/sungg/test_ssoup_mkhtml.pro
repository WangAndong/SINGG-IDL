PRO test_ssoup_mkhtml
  wd        = '~/SUNGG/OUPIPE/J0335-24/'
  wd        = '~/SUNGG/OUPIPE/J0406-52/'
  basedir   = wd
  srcdir    = wd
  outdir    = 'HTML'
  fili      = 'ssoup.in'
  flog      = 'test.log'
  ll        = -1
  ssoup_inputs, fili, ll, hname, fimages_in, fmasks_in, mbadval_in, $
                  fimages_out, fmask_out, fmask_sky, mbadval_out, $
                  fprofs_out, fbox, fjpg_low, fjpg_high, fjpg_mlow1, fjpg_migh1, $
                  fjpg_mlow2, fjpg_mhigh2, fjpg_mlow3, fjpg_mhigh3, $
                  fjpg_imlow1, fjpg_imigh1, fjpg_imlow2, fjpg_imhigh2, $
                  fjpg_imlow3, fjpg_imhigh3, fcompare, scalprof, fcalprof, $
                  scalprof0, fcalprof0, profjpg, profps, hafuvjpg, hafuvps, hafuvjpg0, hafuvps0, $
                  status
  ssoup_mkhtml, ll, srcdir, basedir, outdir, hname, $
                  fjpg_low, fjpg_high, fjpg_mlow1, fjpg_migh1, $
                  fjpg_mlow2, fjpg_mhigh2, fjpg_mlow3, fjpg_mhigh3, $
                  fjpg_imlow1, fjpg_imigh1, fjpg_imlow2, fjpg_imhigh2, $
                  fjpg_imlow3, fjpg_imhigh3, fcompare, scalprof, fcalprof, $
                  scalprof0, fcalprof0, profjpg, profps, $
                  hafuvjpg, hafuvps, hafuvjpg0, hafuvps0, /uselink
END 
