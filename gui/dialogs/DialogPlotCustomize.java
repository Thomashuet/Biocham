package fr.inria.contraintes.biocham.dialogs;

import fr.inria.contraintes.biocham.BiochamMainFrame;
import fr.inria.contraintes.biocham.BiochamModel;
import fr.inria.contraintes.biocham.customComponents.BiochamPlot;
import fr.inria.contraintes.biocham.customComponents.DatasetCard;
import fr.inria.contraintes.biocham.plotting.DatasetProperties;
import fr.inria.contraintes.biocham.plotting.DatasetPropertiesPanel;
import fr.inria.contraintes.biocham.plotting.PlotProperties;
import fr.inria.contraintes.biocham.utils.Icons;
import fr.inria.contraintes.biocham.utils.Utils;

import java.awt.BorderLayout;
import java.awt.CardLayout;
import java.awt.Color;
import java.awt.Container;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.GridLayout;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.util.ArrayList;

import javax.swing.BorderFactory;
import javax.swing.ButtonGroup;
import javax.swing.DefaultListModel;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JFormattedTextField;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JSeparator;
import javax.swing.Spring;
import javax.swing.SpringLayout;
import javax.swing.SwingUtilities;







public class DialogPlotCustomize extends JDialog implements MouseListener{

	
	
	
	
	private BiochamModel model;
	private Container content;
	private BiochamPlot biochamPlot;
	final ButtonGroup group=new ButtonGroup();
	JFormattedTextField xRangeText,yRangeText;
	JPanel options;
    final static String PLOTPANEL = "       Plot   ";
    final static String DATASETPANEL = "   Data Sets   ";
    final static Color HOVER_COLOR=Color.BLUE.darker();   
    JLabel plotOptions, datasetOptions;
	int clicked=0;
	DefaultListModel listModel;
	JPanel cards,card1,card2;	
	DatasetPropertiesPanel pp;
	JCheckBox barsOnOff,impulsesOnOff,gridOnOff,linesOnOff,logXAxisOnOff,logYAxisOnOff;
	JButton addNew,remove;
	PlotProperties plotProperties;
	ArrayList<DatasetProperties> datasetProperties;
	JFormattedTextField titleText,xAxisText,yAxisText,xTicksText,yTicksText;
	JRadioButton rb1,rb2,rb3,rb4,autoRange;
	JLabel choice;
	JPanel buttons;
		
	
	
	
	public DialogPlotCustomize(JFrame parent, BiochamModel m, BiochamPlot bp){
		  
		super (parent, true);		
		boolean b=SwingUtilities.isEventDispatchThread();
		model=m;
		biochamPlot=bp;		
		initComponents();      
	    pack();
	}
	
	
	
	private void initComponents() {
		
		setTitle("Plot Customize Dialog");		
	    content = getContentPane();
	    content.setLayout(new BorderLayout());
	    
	    JPanel contents=new JPanel(new BorderLayout());
	    contents.setBackground(Utils.backgroundColor);
	    contents.setBorder(BorderFactory.createLineBorder(Utils.backgroundColor));
	    
	    JPanel menu=new JPanel(new GridLayout(7,0));
	    menu.setBackground(Utils.backgroundColor);
	    menu.setBorder(BorderFactory.createRaisedBevelBorder());
	    
	    plotOptions=new JLabel(PLOTPANEL);	    
	    choice=plotOptions;
	    plotOptions.setForeground(Color.blue.darker());
	    plotOptions.addMouseListener(this);
	    datasetOptions=new JLabel(DATASETPANEL);
	    datasetOptions.setForeground(Color.blue.darker());
	    datasetOptions.addMouseListener(this);	    
	    JLabel icon =new JLabel(Icons.icons.get("customize.png"+1.8));
	    menu.add(plotOptions);
	    menu.add(datasetOptions);
	    JSeparator s1=new JSeparator(),s2=new JSeparator(),
	    s3=new JSeparator(),s4=new JSeparator();
	    s1.setBackground(Utils.backgroundColor);
	    s2.setBackground(Utils.backgroundColor);
	    s3.setBackground(Utils.backgroundColor);
	    s2.setVisible(false);
	    s1.setVisible(false);
	    s3.setVisible(false);
	    s4.setBackground(Utils.backgroundColor);
	    s4.setVisible(false);
	    menu.add(s1);
	    menu.add(s2);
	    menu.add(icon);
	    menu.add(s3);
	    menu.add(s4);   
	    contents.add(menu,BorderLayout.WEST);
	    
	    	        
	   
		
	    
		buttons=new JPanel();
		JButton apply=new JButton("Apply");
		apply.addActionListener(new ActionListener() { public void actionPerformed(ActionEvent event) { windowAction(event);}});
		apply.setActionCommand("applyPlotSettings");
		JButton cancel=new JButton("Close");
		cancel.addActionListener(new ActionListener() { public void actionPerformed(ActionEvent event) { windowAction(event);}});
		cancel.setActionCommand("close");
		buttons.add(apply);
		buttons.add(cancel);
		buttons.setBackground(Utils.backgroundColor);
		buttons.setBorder(BorderFactory.createRaisedBevelBorder());
		contents.add(buttons,BorderLayout.SOUTH);
		
		options=new JPanel(new CardLayout());
		card1=createPlotCard();
		card2=new DatasetCard(biochamPlot,buttons,this);
		options.add(card1,PLOTPANEL);
		options.add(card2,DATASETPANEL);		
		contents.add(options,BorderLayout.CENTER);
		    
		content.add(contents,BorderLayout.CENTER);
		JFrame frame=BiochamMainFrame.frame;
		Point pos = frame.getLocationOnScreen();
		setLocation(pos.x+frame.getSize().width/2-185,pos.y+frame.getSize().height/2-140);
		setResizable(true);
		setSize(new Dimension(600, 400));	    
		setLocationRelativeTo(frame);
		setVisible(true);	     
	}


	


	private JPanel createPlotCard(){
		
		JPanel panel=new JPanel();
		SpringLayout sl=new SpringLayout();
		panel.setLayout(sl);
		
			//TITLE
		 	JLabel title=new JLabel("Title:");
		 	titleText=new JFormattedTextField(biochamPlot.getDrawingArea().getPlot().getName());
		 	titleText.setName("plotTitle");
		    titleText.setColumns(25);
		    panel.add(title);
		    panel.add(titleText);
		    //xAxis yAxis LABELS
		    JLabel xAxis=new JLabel("X Label:");
		    xAxisText=new JFormattedTextField(biochamPlot.getDrawingArea().getPlot().getXLabel());
		    xAxisText.setName("xLabel");
		    xAxisText.setColumns(9);
		    JLabel yAxis=new JLabel("Y Label:");
		    yAxisText=new JFormattedTextField(biochamPlot.getDrawingArea().getPlot().getYLabel());
		    yAxisText.setName("yLabel");
		    yAxisText.setColumns(9);
		    panel.add(xAxis);
		    panel.add(xAxisText);
		    panel.add(yAxis);
		    panel.add(yAxisText);
		    //xTicks yTicks VALUES
		    JLabel xTicks=new JLabel("X Ticks:");
		    JLabel yTicks=new JLabel("Y Ticks:");
		    panel.add(xTicks);
		    panel.add(yTicks);
		    String xticks="",yticks="";		
		    if(biochamPlot.getDataSetsList().size()>0){
			    ArrayList<String> xTicksList=biochamPlot.getDataSetsList().get(0).getXTicks();
			    int siz1=xTicksList.size();
			    ArrayList<String> yTicksList=biochamPlot.getDataSetsList().get(0).getYValues();
			    int siz2=yTicksList.size();
			    for(int i=0;i<xTicksList.size();i++){
			    	xticks+=xTicksList.get(i);
			    	if(i<xTicksList.size()-1){
			    		xticks+=" , ";
			    	}
			    }	    	   
			    for(int i=0;i<yTicksList.size();i++){
			    	yticks+=yTicksList.get(i);
			    	if(i<yTicksList.size()-1){
			    		yticks+=" , ";
			    	}
			    }
		    }
		    xTicksText=new JFormattedTextField(xticks);
		    xTicksText.setColumns(9);
		    yTicksText=new JFormattedTextField(yticks);
		    yTicksText.setColumns(9);
		    panel.add(xTicksText);
		    panel.add(yTicksText);
		    //MARKS
		    JLabel marks=new JLabel("Marks:");
		    panel.add(marks);
		    rb1=new JRadioButton("none");
		    panel.add(rb1);
		    rb1.setSelected(true);
		    group.add(rb1);
		    rb2=new JRadioButton("points");
		    panel.add(rb2);
		    group.add(rb2);
		    rb3=new JRadioButton("dots");
		    panel.add(rb3);
		    group.add(rb3);
		    rb4=new JRadioButton("various");
		    panel.add(rb4);
		    group.add(rb4);
		   // JRadioButton rb5=new JRadioButton("pixels");
		   // panel.add(rb5);
		  //  group.add(rb5);
		    //AutoRange xRange yRange
		    JLabel xRange=new JLabel("X Range:");
		    JLabel yRange=new JLabel("Y Range:");
		   		    
		    xRangeText=new JFormattedTextField(String.valueOf(biochamPlot.getDrawingArea().getPlot().getXRange()[0])+","+String.valueOf(biochamPlot.getDrawingArea().getPlot().getXRange()[1]));
		    xRangeText.setColumns(8);
		    yRangeText=new JFormattedTextField(String.valueOf(biochamPlot.getDrawingArea().getPlot().getYRange()[0]+","+String.valueOf(biochamPlot.getDrawingArea().getPlot().getYRange()[1])));
		    yRangeText.setColumns(8);
		    xRangeText.setEditable(false);
		    yRangeText.setEditable(false);
		   	    
		    autoRange=new JRadioButton("AutoRange");
		    autoRange.setSelected(true);
		    autoRange.addItemListener(new ItemListener(){
		    	public void itemStateChanged(ItemEvent e) {
					 if(e.getSource() instanceof JRadioButton){					 
						   	if(e.getStateChange()==ItemEvent.DESELECTED){
						   		xRangeText.setEditable(true);
						   		xRangeText.setEnabled(true);
						   		yRangeText.setEditable(true);
						   		yRangeText.setEnabled(true);
						   	}else if(e.getStateChange()==ItemEvent.SELECTED){
						   		xRangeText.setEnabled(false);
						   		xRangeText.setEditable(false);
						   		yRangeText.setEditable(false);
						   		yRangeText.setEnabled(false);
						   	}
					   }				
		    	}
		    });
		    panel.add(autoRange);
		    panel.add(xRange);
		    panel.add(yRange);
		    panel.add(xRangeText);
		    panel.add(yRangeText);	
		    //GRID
		    gridOnOff=new JCheckBox("Grid");
		    gridOnOff.setSelected(true);
		    panel.add(gridOnOff);
		    //BARS
		    barsOnOff=new JCheckBox("Bars");
		    barsOnOff.setSelected(false);
		    barsOnOff.addItemListener(new ItemListener(){

				public void itemStateChanged(ItemEvent e) {
					if(e.getStateChange()==ItemEvent.SELECTED){
						impulsesOnOff.setSelected(false);
					}				
				}});
		    panel.add(barsOnOff);
		    //IMPULSES
		    impulsesOnOff=new JCheckBox("Impulses");
		    impulsesOnOff.setSelected(false);
		    impulsesOnOff.addItemListener(new ItemListener(){

				public void itemStateChanged(ItemEvent e) {
					if(e.getStateChange()==ItemEvent.SELECTED){
						barsOnOff.setSelected(false);
					}				
				}});
		    panel.add(impulsesOnOff);
		    //LINES
		    linesOnOff=new JCheckBox("Lines");
		    linesOnOff.setSelected(true);
		    panel.add(linesOnOff);
		    //LOG xAxis
		    logXAxisOnOff=new JCheckBox("Log XAxis");
		    logXAxisOnOff.setSelected(false);
		    panel.add(logXAxisOnOff);
		    //LOG yAxis
		    logYAxisOnOff=new JCheckBox("Log YAxis");
		    logYAxisOnOff.setSelected(false);
		    panel.add(logYAxisOnOff);
		    
		    JComponent[] leftComponents=new JComponent[8];
		    leftComponents[0]=title;
		    leftComponents[1]=xAxis;
		    leftComponents[2]=xTicks;
		    leftComponents[3]=marks;
		    leftComponents[4]=autoRange;
		    leftComponents[5]=gridOnOff;
		    leftComponents[6]=barsOnOff;
		    leftComponents[7]=xRange;
		    sl.putConstraint(SpringLayout.WEST, title, 10,SpringLayout.WEST , panel);			    
		    sl.putConstraint(SpringLayout.NORTH, title, 20,SpringLayout.NORTH , panel);	
		    sl.putConstraint(SpringLayout.NORTH, titleText, 20,SpringLayout.NORTH , panel);	
		    
		    sl.putConstraint(SpringLayout.WEST, xAxis, 10,SpringLayout.WEST , panel);
		    sl.putConstraint(SpringLayout.NORTH, xAxis, 15,SpringLayout.SOUTH , title);
		    sl.putConstraint(SpringLayout.NORTH, xAxisText, 0,SpringLayout.NORTH , xAxis);
		    sl.putConstraint(SpringLayout.NORTH, yAxis, 0,SpringLayout.NORTH , xAxis);
		    sl.putConstraint(SpringLayout.NORTH, yAxisText, 0,SpringLayout.NORTH , xAxis);
		    
		    sl.putConstraint(SpringLayout.WEST, xTicks, 10,SpringLayout.WEST , panel);
		    sl.putConstraint(SpringLayout.NORTH, xTicks, 15,SpringLayout.SOUTH , xAxis);
		    sl.putConstraint(SpringLayout.NORTH, xTicksText, 0,SpringLayout.NORTH , xTicks);
		    sl.putConstraint(SpringLayout.NORTH, yTicks, 0,SpringLayout.NORTH , xTicks);
		    sl.putConstraint(SpringLayout.NORTH, yTicksText, 0,SpringLayout.NORTH , xTicks);
		    
		    sl.putConstraint(SpringLayout.WEST, marks, 10,SpringLayout.WEST , panel);
		    sl.putConstraint(SpringLayout.NORTH, marks, 26,SpringLayout.SOUTH , xTicks);
		    sl.putConstraint(SpringLayout.NORTH, rb1, 0,SpringLayout.NORTH , marks);
		    sl.putConstraint(SpringLayout.NORTH, rb2, 0,SpringLayout.NORTH , marks);
		    sl.putConstraint(SpringLayout.NORTH, rb3, 0,SpringLayout.NORTH , marks);
		    sl.putConstraint(SpringLayout.NORTH, rb4, 0,SpringLayout.NORTH , marks);
		    
		    sl.putConstraint(SpringLayout.WEST, autoRange, 6,SpringLayout.WEST , panel);
		    sl.putConstraint(SpringLayout.NORTH, autoRange, 13,SpringLayout.SOUTH , marks);
		    
		    sl.putConstraint(SpringLayout.WEST, xRange, 25,SpringLayout.WEST , panel);
		    sl.putConstraint(SpringLayout.NORTH, xRange, 10,SpringLayout.SOUTH , autoRange);
		    sl.putConstraint(SpringLayout.NORTH, xRangeText, 0,SpringLayout.NORTH , xRange);
		    sl.putConstraint(SpringLayout.NORTH, yRange, 0,SpringLayout.NORTH , xRange);
		    sl.putConstraint(SpringLayout.NORTH, yRangeText, 0,SpringLayout.NORTH , xRange);
		    		    
		    sl.putConstraint(SpringLayout.WEST, gridOnOff, 10,SpringLayout.WEST , panel);
		    sl.putConstraint(SpringLayout.NORTH, gridOnOff, 35,SpringLayout.SOUTH , xRange);
		    sl.putConstraint(SpringLayout.NORTH, impulsesOnOff, 0,SpringLayout.NORTH , gridOnOff);
		    sl.putConstraint(SpringLayout.NORTH, logXAxisOnOff, 0,SpringLayout.NORTH , gridOnOff);
		    
		    sl.putConstraint(SpringLayout.WEST, barsOnOff, 10,SpringLayout.WEST , panel);
		    sl.putConstraint(SpringLayout.NORTH, barsOnOff, 6,SpringLayout.SOUTH , gridOnOff);
		    sl.putConstraint(SpringLayout.NORTH, linesOnOff, 0,SpringLayout.NORTH , barsOnOff);
		    sl.putConstraint(SpringLayout.NORTH, logYAxisOnOff, 0,SpringLayout.NORTH , barsOnOff);
		    
			Spring maxSpring = Spring.constant(10);			
			for (int i=0;i<leftComponents.length;++i){
				maxSpring = Spring.max(maxSpring, Spring.sum(sl.getConstraints(leftComponents[i]).getWidth(),Spring.constant(10)));				
		    }
			
			JComponent[] rightComponents1=new JComponent[7];
			rightComponents1[0]=titleText;
			rightComponents1[1]=xAxisText;
			rightComponents1[2]=xTicksText;
			rightComponents1[3]=rb1;
			rightComponents1[4]=xRangeText;
			rightComponents1[5]=impulsesOnOff;
			rightComponents1[6]=linesOnOff;		
		    for (int i=0;i<rightComponents1.length;++i){		    	
		    	sl.putConstraint(SpringLayout.WEST, rightComponents1[i], maxSpring, SpringLayout.WEST, panel);		    	
		    }	    	
		    maxSpring = Spring.constant(25);		
			for (int i=1;i<rightComponents1.length;++i){
				maxSpring = Spring.max(maxSpring, Spring.sum(sl.getConstraint(SpringLayout.WEST,rightComponents1[i]),Spring.sum(sl.getConstraints(rightComponents1[i]).getWidth(),Spring.constant(10))));				
		    }
			
			sl.putConstraint(SpringLayout.WEST, rb2, 10, SpringLayout.EAST, rb1);	
			sl.putConstraint(SpringLayout.WEST, rb3, 10, SpringLayout.EAST, rb2);		
			sl.putConstraint(SpringLayout.WEST, rb4, 10, SpringLayout.EAST, rb3);	
		    
		    JComponent[] rightComponents2=new JComponent[5];
			rightComponents2[0]=yAxis;
			rightComponents2[1]=yTicks;
			rightComponents2[2]=yRange;
			rightComponents2[3]=logXAxisOnOff;
			rightComponents2[4]=logYAxisOnOff;
			for (int i=0;i<rightComponents2.length;++i){		    	
		    	sl.putConstraint(SpringLayout.WEST, rightComponents2[i], maxSpring, SpringLayout.WEST, panel);		    	
		    }	    	
			maxSpring = Spring.constant(40);		
			for (int i=0;i<rightComponents2.length;++i){
				maxSpring = Spring.max(maxSpring, Spring.sum(sl.getConstraint(SpringLayout.WEST,rightComponents2[i]),Spring.sum(sl.getConstraints(rightComponents2[i]).getWidth(),Spring.constant(10))));				
		    }
			sl.putConstraint(SpringLayout.WEST, yAxisText, 10, SpringLayout.EAST, yAxis);		    	
			sl.putConstraint(SpringLayout.WEST, yTicksText, 10, SpringLayout.EAST, yTicks);
			sl.putConstraint(SpringLayout.WEST, yRangeText, 10, SpringLayout.EAST, yRange);
		    
	    	
		
		maxSpring=null;
		return panel;
	}
	

	public void mouseClicked(MouseEvent e) {
		if(e.getSource() instanceof JLabel){
			
			choice=(JLabel)e.getSource();
			choice.setForeground(Color.black);
			if(choice.getText().contains("Data")){
				clicked=1;
				choice.setText(DATASETPANEL);
				plotOptions.setForeground(Color.blue.darker());
			}else{
				clicked=0;
				choice.setText(PLOTPANEL);	
				datasetOptions.setForeground(Color.blue.darker());
			}			
			CardLayout cl=(CardLayout)options.getLayout();
			cl.show(options,choice.getText());		
		}		
	}
	
	
	
	public void mouseEntered(MouseEvent e) {
		if(e.getSource() instanceof JLabel){
			JLabel l=(JLabel)e.getSource();
			if(clicked==0){
				if(l.getText().contains("Plot")){
					l.setForeground(Color.black);
					l.setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
				}else{
					l.setForeground(Color.black);
					l.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
				}
			}else{
				if(l.getText().contains("Data")){
					l.setForeground(Color.black);
					l.setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
				}else{
					l.setForeground(Color.black);
					l.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
				}
			}	
		}
		
	}
	public void mouseExited(MouseEvent e) {
		if(e.getSource() instanceof JLabel){
			JLabel l=(JLabel)e.getSource();
			if(clicked==0){
				if(l.getText().contains("Plot")){
					l.setForeground(Color.black);
				}else{
					l.setForeground(Color.blue.darker());
				}
			}else{
				if(l.getText().contains("Data")){
					l.setForeground(Color.black);
				}else{
					l.setForeground(Color.blue.darker());
				}
			}	
	        l.setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));			
		}
	}
	public void mousePressed(MouseEvent e) {	
	}
	public void mouseReleased(MouseEvent e) {
	}	
	public PlotProperties getPlotProperties() {
		return plotProperties;
	}
	public void setPlotProperties(PlotProperties plotProperties) {
		this.plotProperties = plotProperties;
	}
	
	
	
protected void windowAction(Object actionCommand) {
		

		boolean closeWindow = false;
	    
		String cmd = null;
	    if (actionCommand != null) {
	    	if (actionCommand instanceof ActionEvent) {
	    		cmd = ((ActionEvent)actionCommand).getActionCommand();
	         } else {
	        	 cmd = actionCommand.toString();
	         }
	    }
	    if (cmd == null) {
	         // do nothing
	    }else if(cmd.equals("close")){
	    	closeWindow=true;
	    }else if(cmd.equals("applyPlotSettings")){
	    	
	    	if(choice.getText().contains("Data")){
	    		
	    		DatasetProperties pp=((DatasetCard)card2).getDatasetPanel().getDatasetProperties(false);
	    		if(pp!=null){
	    			biochamPlot.applyDataSetProperties(pp);
	    		}
	    		
	    		
	    	}else if(choice.getText().contains("Plot")){
	    		plotProperties=new PlotProperties();	    		    	
		    	plotProperties.setPlotTitle(titleText.getText());
		    	plotProperties.setXLabel(xAxisText.getText());
		    	plotProperties.setYLabel(yAxisText.getText());
		    	plotProperties.setXTicks(xTicksText.getText());
		    	plotProperties.setYTicks(yTicksText.getText());
		    	if(rb1.isSelected()){
		    		plotProperties.setMarks("none");
		    	}else if(rb2.isSelected()){
		    		plotProperties.setMarks("points");
		    	}else if(rb3.isSelected()){
		    		plotProperties.setMarks("dots");
		    	}else if(rb4.isSelected()){
		    		plotProperties.setMarks("various");
		    	}		    	
		    	if(autoRange.isSelected()){
		    		plotProperties.setAutorange(true);
		    	}else{
		    		plotProperties.setAutorange(false);
		    		plotProperties.setXRange(xRangeText.getText());
		    		plotProperties.setYRange(yRangeText.getText());
		    	}
		    	if(barsOnOff.isSelected()){
		    		plotProperties.setBars(true);
		    	}else{
		    		plotProperties.setBars(false);
		    	}
		    	if(impulsesOnOff.isSelected()){
		    		plotProperties.setImpulses(true);
		    	}else{
		    		plotProperties.setImpulses(false);
		    	}
		    	if(gridOnOff.isSelected()){
		    		plotProperties.setGrid(true);
		    	}else{
		    		plotProperties.setGrid(false);
		    	}
		    	if(linesOnOff.isSelected()){
		    		plotProperties.setLines(true);
		    	}else{
		    		plotProperties.setLines(false);
		    	}
		    	if(logXAxisOnOff.isSelected()){
		    		plotProperties.setXLogAxis(true);
		    	}else{
		    		plotProperties.setXLogAxis(false);
		    	}
		    	if(logYAxisOnOff.isSelected()){
		    		plotProperties.setYLogAxis(true);
		    	}else{
		    		plotProperties.setYLogAxis(false);
		    	}		    	
		    	biochamPlot.applyPlotProperties(plotProperties);	    		
	    	}   	
	    }	    
	    if (closeWindow) {
	    	setVisible(false);
	    	dispose();
	    }
	}



public JPanel getButtons() {
	return buttons;
}



public void setButtons(JPanel buttons) {
	this.buttons = buttons;
}

}
