# weather_forecaster

An OTP application built to consume the darksky api to fetch weather data.

# Usage

To start the app locally, run the following

'''
 make deps compile dev
'''

To reload the city mappings, use the following 

'''
curl -w "\n" -X POST http://localhost:8080/reload_city_list
'''

# Issues
