import javax.swing.*;
import javax.swing.event.*;
import java.awt.*;
import java.awt.event.*;
import java.io.*;

import java.util.Properties;
import org.omg.CORBA.ORB;
import org.omg.PortableServer.POA;
import org.omg.PortableServer.POAHelper;
import org.omg.CosNaming.NameComponent;
import org.omg.CosNaming.NamingContext;
import org.omg.CosNaming.NamingContextHelper;


public class BoxAdminWindow implements AdminPanelInterface{
// private JFrame frame = new JFrame("Competitor");
//sezione JSlider
private JSlider sliderTyreUsury;
private JSlider sliderFuelTank;
private JLabel valueFuelTank;
private JSlider sliderGasLevel;
private JSlider sliderSpeed;
//sezione JLabel
private JLabel labelUsuryTyre;
private JLabel valuePerCentUsury;
private JLabel labelGasLevel;
private JLabel valueLevelFuel;
private JLabel valueSpeed;
private JLabel labelTyre = new JLabel("Tyre type : ");
// private JLabel labelStrategy = new JLabel("Driver Strategy after Pitstop : ");
// private JLabel labelLap = new JLabel("PitStop lap : ");
// private JLabel labelFuel = new JLabel("Pitsop Fuel : ");
private JLabel labelBox = new JLabel("Box Strategy : ");
private JLabel labelName = new JLabel("Name : ");
private JLabel labelSurname = new JLabel("Surname : ");
private JLabel labelStable = new JLabel("Racing Stable : ");
private JLabel labelMixture = new JLabel("Mixture : ");
// private JLabel labelTyrePitStop = new JLabel("Mixture pit stop : ");
private JLabel labelVel = new JLabel("Full Speed : ");
private JLabel labelCapacityFuel = new JLabel("Fuel Capacity : ");
private JLabel labelstrps = new JLabel("Driver Strategy : ");
private JLabel labelAcc = new JLabel("Speedup : ");
private JLabel velLab = new JLabel(" km/h");
// private JLabel labelVel = new JLabel("Velocita' massima : ");
//sezione combobox
private JComboBox comboTyre;
private JComboBox comboStrategy;
private JComboBox comboPitStop;
private JComboBox comboTypeTyre;
private JComboBox comboBox;
private JComboBox comboStrategypitstop;
//altri parametri
private int intTyre;
private int intStrategy;
//sezione spinner giro di fermata
private Integer valueLap;// = new Integer(12); 
private Integer minLap;// = new Integer(0);
private Integer maxLap;// = new Integer(100); 
private Integer step = new Integer(1);;// = new Integer(1); 
private SpinnerNumberModel modelLap;// = new SpinnerNumberModel(valueLap, minLap, maxLap, step); 
//sezione spinner Quantità di benzina
private Integer valueFuel;// = new Integer(100 - sliderGasLevel.getValue()); 
private Integer minFuel;// = new Integer(0);
private Integer maxFuel;// = new Integer(100 - sliderGasLevel.getValue()); 
private SpinnerNumberModel modelFuel;// = new SpinnerNumberModel(valueFuel, minFuel, maxFuel, step); private Integer valueLap;// = new Integer(12); 
//sezione spinner grandezza serbatoio
// private Integer valueSerbatorio = new Integer(12); 
private Integer minTank = new Integer(200);
private Integer maxTank= new Integer(400); 
private JLabel labelTank = new JLabel (" L (200..400)");
// private Integer step;// = new Integer(1); 
private SpinnerNumberModel modelTank;
private SpinnerNumberModel modelAcc;
private JLabel valueAcc = new JLabel(" m/s^2");
//sezione panel
private JPanel carPanel;
private JPanel boxPanel;
private JPanel buttonPanel;
private JPanel dataPanel;
//sezione JSpinner
private JSpinner jsLap;// = new JSpinner(modelLap);
private JSpinner jsFuel;// = new JSpinner(modelFuel);
private JSpinner jsVelocita;
private JSpinner jsAcc;
//sezione GridBagConstraints
private GridBagConstraints carConfigurationGrid;
private GridBagConstraints boxConfigurationGrid;
private GridBagConstraints driverConfigurationGrid;
//Sezione button
private JButton resetButton;
private JButton startButton;
//sezione JTextField
private JTextField textName = new JTextField("Pippo", 20);
private JTextField textSurname = new JTextField("Pluto", 20);
private JTextField textTeam = new JTextField("Ferrari", 20);
//sezione stringhe per file xml
String scuderia = new String("<team>Ferrari</team>");
String nome = new String("<firstname>Pippo</firstname>");
String cognome = new String("<lastname>Pluto<lastname>");
Double valueUsuryDouble;
Integer valueFuelInt;
String tyreUsuryString = new String("<tyreusury>0.15</tyreusury>");
String gasolineString = new String ("<gasolinelevel>50</gasolinelevel>");

// private String stringPitStop = new String("<pitstoptt>Sun</pitstoptt>");
private String stringTipoGomme = new String("<mixture>Soft</mixture>");
// private String stringTipoGommePS = new String("<mixtureps>Soft</mixtureps>");
private String stringGomme = new String("<type_tyre>Sun</type_tyre>");
private String stringStyle = new String("<engine>NORMAL</engine>");
private String stringStrategy = new String("<boxStrategy>NORMAL</boxStrategy>");

// private String stringStrategyPS = new String("<engineps>Normal</engineps>");
// private String stringStrategypitstop = new String("<engineps>Normal</engineps>");
// private String gasolineStringPS = new String("<pitstopGasolineLevel>50</pitstopGasolineLevel>");
// private String pitstopStringLap= new String("<pitstopLaps>12</pitstopLaps>");
private String stringSerbatoio = new String("<gastankcapacity>200</gastankcapacity>");
private String stringMaxAcc = new String("<maxacceleration>7.0</maxacceleration>");
private String stringMaxSpeed = new String("<maxspeed>300</maxspeed>");
// private String stringGommePS = new String("<tyrePS>Sun</tyrePS>");

private String stringId;
public BoxAdminWindow(JFrame frame, String param){
  stringId = param;
  init(frame);
}

class carConfigurationPanel{
public carConfigurationPanel(JPanel panel, GridBagConstraints carConfigurationGrid){
createCar(panel, carConfigurationGrid);
}
public void createCar(JPanel panel, GridBagConstraints carConfigurationGrid){
//impostazione capacità serbatioio
sliderFuelTank = new JSlider(200,400);
sliderFuelTank.setValue(200);
valueFuelTank = new JLabel ("200 L (200-400)");
sliderFuelTank.addChangeListener(new MyChangeAction("L (200-400)", sliderFuelTank, valueFuelTank));
carConfigurationGrid.fill = GridBagConstraints.HORIZONTAL;
		carConfigurationGrid.gridx = 0;
		carConfigurationGrid.gridy = 0;
		carConfigurationGrid.ipady = 5;
		panel.add(labelCapacityFuel, carConfigurationGrid);
carConfigurationGrid.fill = GridBagConstraints.HORIZONTAL;
		carConfigurationGrid.gridx = 1;
		carConfigurationGrid.gridy = 0;
		carConfigurationGrid.ipady = 5;
		panel.add(sliderFuelTank, carConfigurationGrid);
carConfigurationGrid.fill = GridBagConstraints.HORIZONTAL;
		carConfigurationGrid.gridx = 2;
		carConfigurationGrid.gridy = 0;
		carConfigurationGrid.ipady = 5;
		panel.add(valueFuelTank, carConfigurationGrid);
//sezione usura gomme
    sliderTyreUsury = new JSlider();
    sliderTyreUsury.setValue(15);
    valuePerCentUsury = new JLabel("15% ");
    sliderTyreUsury.addChangeListener(new MyChangeAction("%", sliderTyreUsury, valuePerCentUsury));
    labelUsuryTyre  = new JLabel("Tyre Usury");
    carConfigurationGrid.fill = GridBagConstraints.HORIZONTAL;
		carConfigurationGrid.gridx = 0;
		carConfigurationGrid.gridy = 1;
		carConfigurationGrid.ipady = 5;
		panel.add(labelUsuryTyre, carConfigurationGrid);
    carConfigurationGrid.fill = GridBagConstraints.HORIZONTAL;
		carConfigurationGrid.gridx = 1;
		carConfigurationGrid.gridy = 1;
		carConfigurationGrid.ipady = 5;
		panel.add(sliderTyreUsury, carConfigurationGrid);
    carConfigurationGrid.fill = GridBagConstraints.HORIZONTAL;
		carConfigurationGrid.gridx = 2;
		carConfigurationGrid.gridy = 1;
		carConfigurationGrid.ipady = 5;
		panel.add(valuePerCentUsury, carConfigurationGrid);
//sezione valore benzina
    sliderGasLevel = new JSlider(0,(Integer)sliderFuelTank.getValue());
    sliderGasLevel.setValue(50);
    valueLevelFuel = new JLabel("50L ");
    sliderGasLevel.addChangeListener(new MyChangeAction("L",sliderGasLevel,  valueLevelFuel));
    labelGasLevel  = new JLabel("Gasoline Level");
    carConfigurationGrid.fill = GridBagConstraints.HORIZONTAL;
		carConfigurationGrid.gridx = 0;
		carConfigurationGrid.gridy = 2;
		carConfigurationGrid.ipady = 5;
		panel.add(labelGasLevel, carConfigurationGrid);
    carConfigurationGrid.fill = GridBagConstraints.HORIZONTAL;
		carConfigurationGrid.gridx = 1;
		carConfigurationGrid.gridy = 2;
		carConfigurationGrid.ipady = 5;
		panel.add(sliderGasLevel, carConfigurationGrid);
    carConfigurationGrid.fill = GridBagConstraints.HORIZONTAL;
		carConfigurationGrid.gridx = 2;
		carConfigurationGrid.gridy = 2;
		carConfigurationGrid.ipady = 5;
		panel.add(valueLevelFuel, carConfigurationGrid);
//sezione tipologia di gomme montate
String[] gomme = { "Sun", "Rain", "Hard Rain"};
		comboTyre = new JComboBox(gomme);
		comboTyre.setSelectedIndex(0);
		comboTyre.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				JComboBox cb = (JComboBox)e.getSource();
				String s = (String)cb.getSelectedItem();
				stringGomme = new String("<type_tyre>"+s+"</type_tyre>");}
		});
    carConfigurationGrid.fill = GridBagConstraints.HORIZONTAL;
		carConfigurationGrid.gridx = 0;
		carConfigurationGrid.gridy = 3;
		carConfigurationGrid.ipady = 5;
		panel.add(labelTyre, carConfigurationGrid);
    carConfigurationGrid.fill = GridBagConstraints.HORIZONTAL;
		carConfigurationGrid.gridx = 1;
		carConfigurationGrid.gridy = 3;
		carConfigurationGrid.ipady = 5;
		panel.add(comboTyre, carConfigurationGrid);
//sezione mescola delle gomme montate
String[] tipoGomme = { "Soft", "Hard"};
		comboTypeTyre = new JComboBox(tipoGomme);
		comboTypeTyre.setSelectedIndex(0);
		comboTypeTyre.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				JComboBox cb = (JComboBox)e.getSource();
				String s = (String)cb.getSelectedItem();
				stringTipoGomme = new String("<mixture>"+s+"</mixture>");}
		});
carConfigurationGrid.fill = GridBagConstraints.HORIZONTAL;
		carConfigurationGrid.gridx = 0;
		carConfigurationGrid.gridy = 4;
		carConfigurationGrid.ipady = 5;
		panel.add(labelMixture, carConfigurationGrid);
carConfigurationGrid.fill = GridBagConstraints.HORIZONTAL;
		carConfigurationGrid.gridx = 1;
		carConfigurationGrid.gridy = 4;
		carConfigurationGrid.ipady = 5;
		panel.add(comboTypeTyre, carConfigurationGrid);
//impostazione tipo strategia
String[] strategypitstop = {"Normal", "Save", "Aggressive"};
	comboStrategypitstop = new JComboBox(strategypitstop);
	comboStrategypitstop.setSelectedIndex(0);
	comboStrategypitstop.addActionListener(new ActionListener() {
		public void actionPerformed(ActionEvent e) {
			JComboBox cb = (JComboBox)e.getSource();
			String s = (String)cb.getSelectedItem();
 			stringStyle = new String("<engine>"+s.toUpperCase()+"</engine>");
		}
	});
carConfigurationGrid.fill = GridBagConstraints.HORIZONTAL;
		carConfigurationGrid.gridx = 0;
		carConfigurationGrid.gridy = 5;
		carConfigurationGrid.ipady = 5;
		panel.add(labelstrps, carConfigurationGrid);
carConfigurationGrid.fill = GridBagConstraints.HORIZONTAL;
		carConfigurationGrid.gridx = 1;
		carConfigurationGrid.gridy = 5;
		carConfigurationGrid.ipady = 5;
		panel.add(comboStrategypitstop, carConfigurationGrid);
modelAcc = new SpinnerNumberModel(12.000,11.01, 12.99, 0.01);
jsAcc = new JSpinner(modelAcc);
carConfigurationGrid.fill = GridBagConstraints.HORIZONTAL;
		carConfigurationGrid.gridx = 0;
		carConfigurationGrid.gridy = 6;
		carConfigurationGrid.ipady = 5;
		panel.add(labelAcc, carConfigurationGrid);
carConfigurationGrid.fill = GridBagConstraints.HORIZONTAL;
		carConfigurationGrid.gridx = 1;
		carConfigurationGrid.gridy = 6;
		carConfigurationGrid.ipady = 5;
		panel.add(jsAcc, carConfigurationGrid);
carConfigurationGrid.fill = GridBagConstraints.HORIZONTAL;
		carConfigurationGrid.gridx = 2;
		carConfigurationGrid.gridy = 6;
		carConfigurationGrid.ipady = 5;
		panel.add(valueAcc, carConfigurationGrid);
//slider max velocita 250 - 400
sliderSpeed = new JSlider(200,400);
sliderSpeed.setValue(300);
valueSpeed = new JLabel(" 300 km/h");
sliderSpeed.addChangeListener(new MyChangeAction(" km/h",sliderSpeed,  valueSpeed));
carConfigurationGrid.fill = GridBagConstraints.HORIZONTAL;
		carConfigurationGrid.gridx = 0;
		carConfigurationGrid.gridy = 7;
		carConfigurationGrid.ipady = 5;
		panel.add(labelVel, carConfigurationGrid);
carConfigurationGrid.fill = GridBagConstraints.HORIZONTAL;
		carConfigurationGrid.gridx = 1;
		carConfigurationGrid.gridy = 7;
		carConfigurationGrid.ipady = 5;
		panel.add(sliderSpeed, carConfigurationGrid);
carConfigurationGrid.fill = GridBagConstraints.HORIZONTAL;
		carConfigurationGrid.gridx = 2;
		carConfigurationGrid.gridy = 7;
		carConfigurationGrid.ipady = 5;
		panel.add(valueSpeed, carConfigurationGrid);
}
public class MyChangeAction implements ChangeListener{
 private String simboloT;
 private JSlider sliderT;
 private JLabel valoreT;
 MyChangeAction(String p_simbolo, JSlider p_slider, JLabel p_valore){
    simboloT = p_simbolo;
    sliderT = p_slider;
    valoreT = p_valore;
 }
   public void stateChanged(ChangeEvent ce){
     Integer value = sliderT.getValue();
      String str = Integer.toString(value);
      valoreT.setText(str+simboloT);
      Integer a = new Integer((Integer)sliderFuelTank.getValue()-value);
      if (simboloT == "L") {
// 	  if (a < (Integer)jsFuel.getValue()){
// 	      jsFuel.setValue((Integer)sliderFuelTank.getValue() - value);
// 	  }
// modelFuel.setMaximum((Integer)sliderFuelTank.getValue() - (Integer)sliderGasLevel.getValue());	      
stringSerbatoio = new String("<gastankcapacity>"+Integer.toString((Integer)sliderFuelTank.getValue())+"</gastankcapacity>");
gasolineString = new String("<gasolinelevel>"+value.toString()+"</gasolinelevel>");
      }
if(simboloT =="L (200-400)"){sliderGasLevel.setMaximum((Integer)sliderT.getValue());/*
modelFuel.setMaximum((Integer)sliderFuelTank.getValue() - (Integer)sliderGasLevel.getValue());*/
 	
}
    }
  }
}

class driverConfigurationPanel{
driverConfigurationPanel(JPanel dataPanel, GridBagConstraints driverConfigurationGrid){createData(dataPanel, driverConfigurationGrid);}
public void createData(JPanel dataPanel, GridBagConstraints driverConfigurationGrid){
driverConfigurationGrid.fill = GridBagConstraints.HORIZONTAL;
		driverConfigurationGrid.gridx = 0;
		driverConfigurationGrid.gridy = 0;
		driverConfigurationGrid.ipady = 5;
		dataPanel.add(labelName, driverConfigurationGrid);
driverConfigurationGrid.fill = GridBagConstraints.HORIZONTAL;
		driverConfigurationGrid.gridx = 1;
		driverConfigurationGrid.gridy = 0;
		driverConfigurationGrid.ipady = 5;
		dataPanel.add(textName, driverConfigurationGrid);
driverConfigurationGrid.fill = GridBagConstraints.HORIZONTAL;
		driverConfigurationGrid.gridx = 0;
		driverConfigurationGrid.gridy = 1;
		driverConfigurationGrid.ipady = 5;
		dataPanel.add(labelSurname, driverConfigurationGrid);
driverConfigurationGrid.fill = GridBagConstraints.HORIZONTAL;
		driverConfigurationGrid.gridx = 1;
		driverConfigurationGrid.gridy = 1;
		driverConfigurationGrid.ipady = 5;
		dataPanel.add(textSurname, driverConfigurationGrid);
driverConfigurationGrid.fill = GridBagConstraints.HORIZONTAL;
		driverConfigurationGrid.gridx = 0;
		driverConfigurationGrid.gridy = 2;
		driverConfigurationGrid.ipady = 5;
		dataPanel.add(labelStable, driverConfigurationGrid);
driverConfigurationGrid.fill = GridBagConstraints.HORIZONTAL;
		driverConfigurationGrid.gridx = 1;
		driverConfigurationGrid.gridy = 2;
		driverConfigurationGrid.ipady = 5;
		dataPanel.add(textTeam, driverConfigurationGrid);
}
}

class boxConfigurationPanel{
public boxConfigurationPanel(JPanel dataPanel, GridBagConstraints boxConfigurationGrid){
createStrategy(dataPanel, boxConfigurationGrid);
}
private void createStrategy(JPanel dataPanel, GridBagConstraints boxConfigurationGrid){

String[] strategy = {"Normal", "Cautious", "Risky", "Fool"};
	comboBox = new JComboBox(strategy);
	comboBox.setSelectedIndex(0);
	comboBox.addActionListener(new ActionListener() {
		public void actionPerformed(ActionEvent e) {
			JComboBox cb = (JComboBox)e.getSource();
			String s = (String)cb.getSelectedItem();
s=s.toUpperCase();
stringStrategy = new String("<engine>"+s+"</engine>");
System.out.println(s);
		}
	});

//adding component to PitStop configuration
boxConfigurationGrid.fill = GridBagConstraints.HORIZONTAL;
		boxConfigurationGrid.gridx = 0;
		boxConfigurationGrid.gridy = 0;
		boxConfigurationGrid.ipady = 5;
		boxPanel.add(labelBox, boxConfigurationGrid);
boxConfigurationGrid.fill = GridBagConstraints.HORIZONTAL;
		boxConfigurationGrid.gridx = 1;
		boxConfigurationGrid.gridy = 0;
		boxConfigurationGrid.ipady = 5;
		boxPanel.add(comboBox, boxConfigurationGrid);

}
 }

class buttonConfigurationPanel{
public buttonConfigurationPanel(JPanel buttonPanel){
createButton(buttonPanel);
}
public void createButton(JPanel buttonPanel){
startButton = new JButton("Join Competition");
		startButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
			    System.out.println("Creazione file xml");
			    //comando per avviare la competizione.
			    scuderia = new String("<team>"+textTeam.getText()+"</team>");			    
			    nome = new String("<firstname>"+textName.getText()+"</firstname>");
			    cognome = new String("<lastname>"+textSurname.getText()+"</lastname>");
			    valueUsuryDouble = new Double ((double)sliderTyreUsury.getValue() / (double)100);
			    valueFuelInt = new Integer((int)sliderGasLevel.getValue());
			    tyreUsuryString = new String("<tyreusury>"+valueUsuryDouble.toString()+"</tyreusury>");
			    gasolineString = new String("<gasolinelevel>"+valueFuelInt.toString()+"</gasolinelevel>");
stringMaxAcc = new String("<maxacceleration>"+jsAcc.getValue().toString()+"</maxacceleration>");
stringMaxSpeed = new String("<maxspeed>"+Integer.toString((Integer)sliderSpeed.getValue())+"</maxspeed>");
boolean ret = false;
ret = writerCompetitorXML();
if (ret=true) {System.out.println("Scrittura riuscita");}
else {System.out.println("Scrittura non riuscita");}
			}
		});
resetButton = new JButton("Reset Field");
		resetButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
			      resetInfo();			}
		});
//adding button to buttonPane
buttonPanel.add(startButton);
buttonPanel.add(resetButton);

}
}

public void init(JFrame frame){
// GridBagConstraints 
carConfigurationGrid = new GridBagConstraints();
dataPanel = new JPanel(new BorderLayout());
dataPanel.setLayout(new GridBagLayout());
dataPanel.setBorder(BorderFactory.createTitledBorder(null, "Competitor Data", javax.swing.border.TitledBorder.DEFAULT_JUSTIFICATION, javax.swing.border.TitledBorder.DEFAULT_POSITION));
driverConfigurationGrid = new GridBagConstraints();

boxPanel= new JPanel(new BorderLayout());
boxPanel.setLayout(new GridBagLayout());
boxPanel.setBorder(BorderFactory.createTitledBorder(null, "Box Configuration", javax.swing.border.TitledBorder.DEFAULT_JUSTIFICATION, javax.swing.border.TitledBorder.DEFAULT_POSITION));
boxConfigurationGrid = new GridBagConstraints();

carPanel = new JPanel(new BorderLayout());
carPanel.setLayout(new GridBagLayout());
carPanel.setBorder(BorderFactory.createTitledBorder(null, "Car Configuration", javax.swing.border.TitledBorder.DEFAULT_JUSTIFICATION, javax.swing.border.TitledBorder.DEFAULT_POSITION));

buttonPanel= new JPanel(new BorderLayout());
buttonPanel.setBorder(BorderFactory.createTitledBorder(null, "Submit & Undo", javax.swing.border.TitledBorder.DEFAULT_JUSTIFICATION, javax.swing.border.TitledBorder.DEFAULT_POSITION));
buttonPanel.setLayout(new FlowLayout());

carConfigurationPanel car = new carConfigurationPanel(carPanel, carConfigurationGrid);
driverConfigurationPanel driver = new driverConfigurationPanel(dataPanel, driverConfigurationGrid);
boxConfigurationPanel box = new boxConfigurationPanel(boxPanel, boxConfigurationGrid);
buttonConfigurationPanel button = new buttonConfigurationPanel(buttonPanel);
frame.add(dataPanel,BorderLayout.NORTH);
frame.add(carPanel, BorderLayout.WEST);
frame.add(boxPanel, BorderLayout.EAST);
frame.add(buttonPanel, BorderLayout.SOUTH);
frame.pack();
frame.setVisible(true);
frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
}
public boolean verifySetting(){return true;}
public void confirmSetting(){}
public void switchPanel(JPanel succPanel){}
public void resetInfo(){
			    System.out.println("Reset Field");
			      sliderTyreUsury.setValue(15);//usura gomme
			      sliderGasLevel.setValue(50);//benzina caricata
			      comboTyre.setSelectedIndex(0);//gomme montate
			      comboBox.setSelectedIndex(0);//tipo di guida
			      comboTypeTyre.setSelectedIndex(0);//mescola gomme montate
			      sliderFuelTank.setValue(200);//capacità serbatoio
			      jsAcc.setValue(12.000);//valore accelerazione
			      textName.setText("Pippo");//nome predefinito
			      textSurname.setText("Pluto");//cognome predefinito
			      textTeam.setText("Ferrari");//scuderia predefinita
			      sliderSpeed.setValue(300);
}
public boolean writerCompetitorXML(){
try{
PrintWriter out;
File f = new File("competitor-"+stringId+".xml");
if (f.exists() == false ) {
out=new PrintWriter(new File("competitor-"+stringId+".xml"));
}
else {
out=new PrintWriter(f);
}
// String item=in.next();
// int number=in.nextInt();
out.println("<?xml version=\"1.0\"?>\n <car_driver>\n<driver>");
// out.println();
// in.close();
out.println(scuderia);
out.println(nome);
out.println(cognome);
out.println("</driver>");
out.println("<car>");
out.println(stringMaxSpeed);
out.println(stringMaxAcc);

// <maxspeed>350</maxspeed>\n<maxacceleration>1.2</maxacceleration>");
out.println(stringSerbatoio);
out.println(stringStyle);  
out.println(tyreUsuryString);
out.println(gasolineString);
// out.println("<gasolinelevel>50.0</gasolinelevel>");
out.println(stringTipoGomme);
// out.println("<model>michelin</model>");
out.println(stringGomme + "\n</car>"); //\n<strategy_car>");
/*out.println(gasolineStringPS);
out.println(pitstopStringLap);
out.println(stringPitStop);
out.println(stringTipoGommePS);
out.println(stringStrategypitstop);*/
// out.println("<pitstopCondition>false</pitstopCondition>");
// out.println("<trim>1</trim>");
// out.println("<pitstop>false</pitstop>\n</strategy_car>\n</car_driver>");
out.println("</car_driver>");	

out.close();
return true;
}
catch(IOException e){
e.printStackTrace();
return false;
}
}

public boolean connect(){
	try {
	
            //initialize orb
            /*Properties props = System.getProperties();
            props.put("org.omg.CORBA.ORBInitialPort", args[1]);
            //Replace MyHost with the name of the host on which you are running the server
            props.put("org.omg.CORBA.ORBInitialHost", args[0]);
            ORB orb = ORB.init(args, props);
	    */
//LETTURA IOR DA FILE
	    FileReader doc=new FileReader("/ior/ior.txt");
BufferedReader bufRead = new BufferedReader(doc);
String ior;    // String that holds current file line
 int count = 0;  // Line number of count 
            
            // Read first line
            ior = bufRead.readLine();
            count++;
            
            // Read through file one line at time. Print line # and line
           while (ior != null){
                ior = ior+bufRead.readLine();
                count++;
            }
            bufRead.close();

//inizializzazione e comunicazione con la logica.
// 	    ORB orb = ORB.init(ior, null);
	    System.out.println("Initialized ORB");

            //Instantiate Servant and create reference
// 	    POA rootPOA = POAHelper.narrow(orb.resolve_initial_references("RootPOA"));
           
            //rootPOA.activate_object(listener);
	    //Echo ref = EchoHelper.narrow(
              //  rootPOA.servant_to_reference(listener));

            //Resolve MessageServer
// 	    org.omg.CORBA.Object obj = orb.string_to_object(ior);
// 	    Echo comp = EchoHelper.narrow(obj);
	/*	
	    Echo msgServer = EchoHelper.narrow(
	        orb.string_to_object("corbaloc:iiop:1.2@"+args[0]+":"+args[1]+"//"+args[2]));*/

            //Register listener reference (callback object) with MessageServer
           
//             System.out.println("Listener registered with MessageServer :" + comp.echoQuarantadue("sono client java - echoQuarantadue"));
//             System.out.println("Listener registered with MessageServer :" + comp.echoString("sono client java - echoString"));
// 	    org.omg.CORBA.IntHolder pippo = new org.omg.CORBA.IntHolder();
// 	    comp.echoProcedure("sono client java - echoProcedure", pippo);
//             System.out.println("Listener registered with MessageServer :");

	    //Activate rootpoa
            //rootPOA.the_POAManager().activate();

            //Wait for messages
            //orb.run();


	} catch (Exception e) {
	    e.printStackTrace();
	}
 return true;
   }




public static void main(String[] args){
JFrame j = new JFrame("Box Admin Window");
BoxAdminWindow boxWindow = new BoxAdminWindow(j, args[0]);
}
}