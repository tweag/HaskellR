CANDLE=candle
LIGHT=light
HEAT=heat
EXTS=-ext WixUtilExtension.dll -ext WixUIExtension.dll -ext WixBalExtension.dll
VERSION=0.2

H-Bundle.exe: H-Bundle.wxs R/R.wxs H/Hb.wxs HP/HP.wxs Cygwin/Cygwin.wxs H/H.msi
	$(CANDLE) H-Bundle.wxs R/R.wxs H/Hb.wxs HP/HP.wxs Cygwin/Cygwin.wxs $(EXTS)
	$(LIGHT) -out H-Bundle.exe H-Bundle.wixobj R.wixobj Hb.wixobj HP.wixobj Cygwin.wixobj $(EXTS)


H/H.msi: H/H.wxs H/HSource.wxs
	$(CANDLE) H/H.wxs H/HSource.wxs H/WixUI_FeatureTree2.wxs $(EXTS)
	$(LIGHT) -out H/H.msi H.wixobj HSource.wixobj WixUI_FeatureTree2.wixobj  -loc H/WixUI_en-us.wxl $(EXTS)

clean: 
	rm H/H.msi *.wixobj *.exe *.wixpdb

H/HSource.wxs:  H-$(VERSION)
	$(HEAT) dir H-$(VERSION) -ag -dr DirectorySources -cg HSources -out H\HSource.wxs
	
