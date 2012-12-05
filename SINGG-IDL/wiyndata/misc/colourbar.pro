PRO colourbar, position, ct, currentct, range, ticks=ticks, $
               charsize=charsize, ticklen=ticklen, minor=minor
   IF NOT(keyword_set(charsize)) THEN charsize=1.0
   IF NOT(keyword_set(ticklen)) THEN ticklen=0.2
   IF NOT(keyword_set(ticks)) THEN ticks=6
   IF NOT(keyword_set(minor)) THEN minor=2
   linepos = [[0,0],[2/256.0,0],[2/256.0,1],[0,1]]
   delta   = [[1,0],[1,0],[1,0],[1,0]]/256.0
   loadct, ct, /silent
   plot, [0], position=position, /nodata, xstyle=5, ystyle=5, $
     xrange=[0,1], yrange=[0,1], /noerase
   FOR ii=0,254 DO polyfill, linepos+ii*delta, color=ii
   polyfill, [[255/256.0,0],[1,0],[1,1],[255/256.0,1]], color=255
   loadct, 0, /silent
   plot, [0], position=position, /nodata, xstyle=1, ystyle=1, $
     xrange=range, yrange=[0,1], /noerase, yticks=1, ytickformat='(A1)', $
     xticks=ticks, charsize=charsize, ticklen=ticklen, xminor=minor
   loadct, currentct, /silent

END
