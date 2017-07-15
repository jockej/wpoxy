bin_PROGRAMS = wpoxyd wpoxyc

wpoxyd$(EXEEXT): .FORCE
	gprbuild server.gpr

wpoxyc$(EXEEXT): .FORCE
	gprbuild client.gpr

.FORCE:
.PHONY: .FORCE
