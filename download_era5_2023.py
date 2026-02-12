import cdsapi

dataset = "derived-era5-land-daily-statistics"

# Days for each month (2023 is not a leap year)
days_in_month = {
    "01": 31,
    "02": 28,
    "03": 31,
    "04": 30,
    "05": 31,
    "06": 30,
    "07": 31,
    "08": 31,
    "09": 30,
    "10": 31,
    "11": 30,
    "12": 31,
}

client = cdsapi.Client()
# This requires a Copernicus API key

for month, num_days in days_in_month.items():
    days = [f"{d:02d}" for d in range(1, num_days + 1)]

    request = {
        "variable": ["2m_temperature"],
        "year": "2023",
        "month": month,
        "day": days,
        "daily_statistic": "daily_mean",
        "time_zone": "utc+00:00",
        "frequency": "6_hourly",
        "area": [44.55, 25.95, 44.35, 26.25],  # Bucharest area
    }

    output_file = f"bucharest_2023_{month}.nc"
    print(f"Downloading {month}/2023 to {output_file}...")

    client.retrieve(dataset, request).download(output_file)
    print(f"  Done: {output_file}")

print("\nAll months downloaded!")
