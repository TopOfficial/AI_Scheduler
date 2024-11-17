from datetime import datetime

# Function to get the current date in 'year-month-day' format
def get_current_date():
    return datetime.now().strftime("%Y-%m-%d")

# Function to get the current time in 24-hour format
def get_current_time_24hr():
    return datetime.now().strftime("%H:%M")