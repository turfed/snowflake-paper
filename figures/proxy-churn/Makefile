# metrics-ip-salted.jsonl is distributed separately.

proxy-churn-windows.csv: .EXTRA_PREREQS = proxy-churn-windows.go
proxy-churn-windows.csv: metrics-ip-salted.jsonl
	go run proxy-churn-windows.go -duration 24h -before 10h -after 40h -tolerance 5m < "$<" > "$@"

.DELETE_ON_ERROR:
