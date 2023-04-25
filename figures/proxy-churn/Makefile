# metrics-ip-salted.jsonl is distributed separately.

proxy-count-decay.pdf: .EXTRA_PREREQS = proxy-count-decay.r ../common.r
proxy-count-decay.pdf: proxy-churn-windows.csv
	Rscript proxy-count-decay.r "$<" "$@"

proxy-churn-windows.csv: .EXTRA_PREREQS = proxy-churn-windows.go
proxy-churn-windows.csv: metrics-ip-salted.jsonl
	go run proxy-churn-windows.go -duration 24h5m -before 10h5m -after 40h5m < "$<" > "$@"

.DELETE_ON_ERROR:
