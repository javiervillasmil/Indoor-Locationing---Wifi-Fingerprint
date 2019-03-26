# Indoor Locationing - Wifi Fingerprint

##
## Summary
Many real world applications need to know the localization of a user in the world to provide their services. Automatic user localization consists of estimating the position of the user (latitude, longitude and altitude) by using an electronic device. While Outdoor localization problem can be solved very accurately thanks to the inclusion of GPS, indoor localization is still an open problem mainly due to the loss of GPS signal in indoor environments.
##

##
## 1.	SCOPE
Investigate the feasibility of using Wifi Fingerprinting to determine a person's location in indoor spaces. Wifi fingerprinting is a method of positioning that uses the signals from multiple WiFi Access Points within a building to determine a specific location, analogously to how GPS uses satellite signals. 
##

##
## 2.	OBJECTIVES
•	Analyze and understand the dataset provided by the Universitat Jaume I to detect possible problems regarding the values of signals for each access point and the methodology used for creating the train and validation set.
•	Define the procedure used to train the models.
•	Evaluate multiple machine learning models to see which produces accurate results.
•	Make predictions of Building ID, Floor, Longitude, and Latitude.
•	Analyze error metrics and detect in which locations the models excels or underperform.
•	Elaborate recommendations on how the result might be improved.
##

##
## 3.	DATA SET
The UJIIndoorLoc database covers three (3) buildings of Universitat Jaume I, and it records the signals of wireless access points received by cellphones according to their location (fingerprint). 
##
