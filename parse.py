import xml.etree.ElementTree as ET
import csv

def find( element, token ):
	return element.findall( ".//ns:%s" % token, namespaces={'ns': ns} )
	
def findtext( element, token ):
	return element.findtext( ".//ns:%s" % token, namespaces={'ns': ns}, default="" )

def parserecord( descr, coord ):
	if descr is '':
		return None
	lines = descr.splitlines()
	map = {}
	for line in lines:
		(key,value) = line.split(":",1)
		map[key] = value.strip()
		#print "%s=>%s" % (key,value)
	(x,y,z) = coord.split(",",2)
	map['Lat']=x
	map['Long']=y
	return map
	
keys = ['Name', 'Mobile number', 'Date and time of receive SMS', 'Date and time of GPS position', 'Battery state', 'VHF telemetry', 'GSM signal level', 'GPS interval', 'VHF interval', 'Temperature', 'Activity', 'Lat', 'Long']

ns = "http://www.opengis.net/kml/2.2"
root = ET.parse( "doc.kml" )

with open("file.csv",'wb') as f:
	writer = csv.DictWriter( f, keys, quoting=csv.QUOTE_ALL )
	writer.writeheader()

	for el in find( root, "Placemark" ):
		map = parserecord( findtext( el, "description" ),  findtext( el, "coordinates" ) )
		if map is not None:
			writer.writerow( map )
	




