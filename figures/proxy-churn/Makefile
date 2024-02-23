proxy-count-decay.pdf: .EXTRA_PREREQS = proxy-count-decay.r ../common.r
proxy-count-decay.pdf: proxy-churn-windows.csv
	Rscript proxy-count-decay.r "$<" "$@"

proxy-churn-windows.csv: .EXTRA_PREREQS = proxy-churn-windows.go
proxy-churn-windows.csv: metrics-ip-salted.jsonl
	go run proxy-churn-windows.go -duration 24h5m -before 5m -after 40h5m < "$<" > "$@"

# metrics-ip-salted.jsonl is distributed separately.
metrics-ip-salted.jsonl: ;
.SECONDARY: metrics-ip-salted.jsonl

.DELETE_ON_ERROR:
