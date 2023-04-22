# metrics-ip-salted.jsonl is distributed separately.

proxy-churn-windows.csv: .EXTRA_PREREQS = proxy-churn-windows.go
proxy-churn-windows.csv: metrics-ip-salted.jsonl
	go run proxy-churn-windows.go -duration 24h5m -before 10h5m -after 40h5m < "$<" > "$@"

.DELETE_ON_ERROR:
