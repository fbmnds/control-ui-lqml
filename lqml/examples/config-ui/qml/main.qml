import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Window 2.15
import QtCharts 2.0

Item {
    id: main
    objectName: "main"
    width: Screen.width
    height: Screen.height

    Column {
        id: column
        spacing: 10
        width: parent.width
        height: parent.height

      	Label {
      	    id: label
            objectName: "label"
            font.pixelSize: 22
            width: parent.width
            height: 40
            
            text: "Control UI"
            verticalAlignment: Text.AlignVCenter
            horizontalAlignment: Text.AlignHCenter
      	}

      	Button {
            id: button
            objectName: "button"
      	    
            width: parent.width
            height: 50
      	    
            text: "Licht"

            function set (clr) {
                background.color = clr
            }
            
            background: Rectangle {
          	radius: 2
          	color: "lightblue"
            }

	    Timer {
       		id: textTimer1
       		interval: 150000
       		repeat: true
       		running: true
       		triggeredOnStart: true
       		onTriggered: Lisp.call(this, "app:werkstattlicht")
   	    }
	    
            onPressed: Lisp.call(this, "app:button-pressed")
      	}
        


	Rectangle {
	    id: rect2
   	    width: parent.width
   	    height: 30
   	    color: "lavender"
	    
   	    Text {
       		id: foo2
       		font.pointSize: 12
       		anchors.verticalCenter: parent.verticalCenter
       		anchors.horizontalCenter: parent.horizontalCenter
       		
       		function set() {
           	    foo2.text = Date()
       		}
   	    }
	    
   	    Timer {
       		id: textTimer2
       		interval: 1000
       		repeat: true
       		running: true
       		triggeredOnStart: true
       		onTriggered: foo2.set()
   	    }	
	}
        
        Rectangle {
	    id: rect3
   	    width: parent.width
   	    height: 200
   	    color: "lavender"


            Canvas
            {
                width: parent.width
                height: parent.width
                onPaint:
                {
                    var x = parent.width / 2
                    var y = parent.height / 2
                    var radius = 70;
                    var startangle = -90
                    var endangle = 30
                    var context = getContext("2d");
                    for(var j=0; j < 3; j++)
                    {
                        context.beginPath();
                        context.moveTo(x, y);
                        context.arc(x, y, radius, (startangle)*(Math.PI/180), (endangle)*(Math.PI/180), false) //x, y, radius, startAngle, endAngle, anticlockwise
                        context.fillStyle = "lightblue"
                        context.fill();
                        startangle += 120
                        endangle += 120
                        context.closePath();
                    }
                }
            }
            //

            
            
            //                    ChartView {
            //                        title: "Line"
            //                        anchors.fill: parent
            //                        antialiasing: false
            //
            //
            //                        LineSeries {
            //                            name: "LineSeries"
            //                            XYPoint { x: 0; y: 0 }
            //                            XYPoint { x: 1.1; y: 2.1 }
            //                            XYPoint { x: 1.9; y: 3.3 }
            //                            XYPoint { x: 2.1; y: 2.1 }
            //                            XYPoint { x: 2.9; y: 4.9 }
            //                            XYPoint { x: 3.4; y: 3.0 }
            //                            XYPoint { x: 4.1; y: 3.3 }
            //                        }
            //                    }
	}
    }
}
