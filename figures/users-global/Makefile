PYTHON = python3

# https://metrics.torproject.org/collector/archive/bridge-descriptors/extra-infos/
BRIDGE_EXTRA_INFOS = \
	bridge-extra-infos-2017-01.tar.xz \
	bridge-extra-infos-2017-02.tar.xz \
	bridge-extra-infos-2017-03.tar.xz \
	bridge-extra-infos-2017-04.tar.xz \
	bridge-extra-infos-2017-05.tar.xz \
	bridge-extra-infos-2017-06.tar.xz \
	bridge-extra-infos-2017-07.tar.xz \
	bridge-extra-infos-2017-08.tar.xz \
	bridge-extra-infos-2017-09.tar.xz \
	bridge-extra-infos-2017-10.tar.xz \
	bridge-extra-infos-2017-11.tar.xz \
	bridge-extra-infos-2017-12.tar.xz \
	bridge-extra-infos-2018-01.tar.xz \
	bridge-extra-infos-2018-02.tar.xz \
	bridge-extra-infos-2018-03.tar.xz \
	bridge-extra-infos-2018-04.tar.xz \
	bridge-extra-infos-2018-05.tar.xz \
	bridge-extra-infos-2018-06.tar.xz \
	bridge-extra-infos-2018-07.tar.xz \
	bridge-extra-infos-2018-08.tar.xz \
	bridge-extra-infos-2018-09.tar.xz \
	bridge-extra-infos-2018-10.tar.xz \
	bridge-extra-infos-2018-11.tar.xz \
	bridge-extra-infos-2018-12.tar.xz \
	bridge-extra-infos-2019-01.tar.xz \
	bridge-extra-infos-2019-02.tar.xz \
	bridge-extra-infos-2019-03.tar.xz \
	bridge-extra-infos-2019-04.tar.xz \
	bridge-extra-infos-2019-05.tar.xz \
	bridge-extra-infos-2019-06.tar.xz \
	bridge-extra-infos-2019-07.tar.xz \
	bridge-extra-infos-2019-08.tar.xz \
	bridge-extra-infos-2019-09.tar.xz \
	bridge-extra-infos-2019-10.tar.xz \
	bridge-extra-infos-2019-11.tar.xz \
	bridge-extra-infos-2019-12.tar.xz \
	bridge-extra-infos-2020-01.tar.xz \
	bridge-extra-infos-2020-02.tar.xz \
	bridge-extra-infos-2020-03.tar.xz \
	bridge-extra-infos-2020-04.tar.xz \
	bridge-extra-infos-2020-05.tar.xz \
	bridge-extra-infos-2020-06.tar.xz \
	bridge-extra-infos-2020-07.tar.xz \
	bridge-extra-infos-2020-08.tar.xz \
	bridge-extra-infos-2020-09.tar.xz \
	bridge-extra-infos-2020-10.tar.xz \
	bridge-extra-infos-2020-11.tar.xz \
	bridge-extra-infos-2020-12.tar.xz \
	bridge-extra-infos-2021-01.tar.xz \
	bridge-extra-infos-2021-02.tar.xz \
	bridge-extra-infos-2021-03.tar.xz \
	bridge-extra-infos-2021-04.tar.xz \
	bridge-extra-infos-2021-05.tar.xz \
	bridge-extra-infos-2021-06.tar.xz \
	bridge-extra-infos-2021-07.tar.xz \
	bridge-extra-infos-2021-08.tar.xz \
	bridge-extra-infos-2021-09.tar.xz \
	bridge-extra-infos-2021-10.tar.xz \
	bridge-extra-infos-2021-11.tar.xz \
	bridge-extra-infos-2021-12.tar.xz \
	bridge-extra-infos-2022-01.tar.xz \
	bridge-extra-infos-2022-02.tar.xz \
	bridge-extra-infos-2022-03.tar.xz \
	bridge-extra-infos-2022-04.tar.xz \
	bridge-extra-infos-2022-05.tar.xz \
	bridge-extra-infos-2022-06.tar.xz \
	bridge-extra-infos-2022-07.tar.xz \
	bridge-extra-infos-2022-08.tar.xz \
	bridge-extra-infos-2022-09.tar.xz \
	bridge-extra-infos-2022-10.tar.xz
.SECONDARY: $(BRIDGE_EXTRA_INFOS)

users-global.pdf: .EXTRA_PREREQS = users-global.r ../common.r
users-global.pdf: userstats-bridge-transport.csv
	Rscript users-global.r $^ "$@"

userstats-bridge-transport.csv: .EXTRA_PREREQS = userstats-bridge-transport
userstats-bridge-transport.csv: $(BRIDGE_EXTRA_INFOS)
	$(PYTHON) userstats-bridge-transport $^ > "$@"

.DELETE_ON_ERROR:
