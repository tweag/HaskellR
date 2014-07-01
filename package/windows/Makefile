CANDLE=candle
LIGHT=light
HEAT=heat
PANDOC=pandoc
EXTS=-ext WixUtilExtension.dll -ext WixUIExtension.dll -ext WixBalExtension.dll
VERSION=0.3.1
HSourceDir=$(shell cygpath -ad H-$(VERSION))
export VERSION
export HSourceDir

HP_INSTALLER = HaskellPlatform-2013.2.0.0-setup.exe
CYGWIN_INSTALLER = setup-x86.exe
R_INSTALLER = R-3.1.0-win.exe

H-Bundle.exe: HP/$(HP_INSTALLER) Cygwin/$(CYGWIN_INSTALLER) R/$(R_INSTALLER)
H-Bundle.exe: H-Bundle.wxs R/R.wxs H/Hb.wxs HP/HP.wxs Cygwin/Cygwin.wxs H/H.msi
	$(CANDLE) H-Bundle.wxs R/R.wxs H/Hb.wxs HP/HP.wxs Cygwin/Cygwin.wxs $(EXTS)
	$(LIGHT) -out H-Bundle.exe H-Bundle.wixobj R.wixobj Hb.wixobj HP.wixobj Cygwin.wixobj $(EXTS)


H/H.msi: H/H.wxs H/HSource.wxs
	$(CANDLE) -dSourceDir=H-$(VERSION) H/H.wxs H/HSource.wxs H/WixUI_FeatureTree2.wxs $(EXTS)
	$(LIGHT) -out H/H.msi H.wixobj HSource.wixobj WixUI_FeatureTree2.wixobj  -loc H/WixUI_en-us.wxl $(EXTS)

clean: 
	-rm H/H.msi *.wixobj *.exe *.wixpdb H/HSource.wxs H/*.pdf

H/HSource.wxs:  H-$(VERSION)
	$(HEAT) dir "H-$(VERSION)" -ag -dr DirectorySources -cg HSource -template:fragment -srd -sfrag -out H/HSource.wxs -var env.HSourceDir

prepare: H-$(VERSION)

H-$(VERSION): 
	mkdir H-$(VERSION)
	cp -r ../../src H-$(VERSION)/

prepare-pdf: H/H-user.pdf H/H-ints.pdf

H/H-user.pdf:
	$(PANDOC) -f markdown -s -S --toc ../../doc/H-user.md -o $@

H/H-ints.pdf:
	$(PANDOC) -f markdown -s -S --toc ../../doc/H-ints.md -o $@

HP/$(HP_INSTALLER):
	wget https://www.haskell.org/platform/download/2013.2.0.0/$(HP_INSTALLER) -O HP/$(HP_INSTALLER)
Cygwin/$(CYGWIN_INSTALLER):
	wget http://cygwin.com/$(CYGWIN_INSTALLER) -O Cygwin/$(CYGWIN_INSTALLER)
R/$(R_INSTALLER):
	wget http://cran.r-project.org/bin/windows/base/$(R_INSTALLER) -O R/$(R_INSTALLER)


