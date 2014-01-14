package fr.inria.contraintes.biocham.dialogs;

import fr.inria.contraintes.biocham.BiochamMainFrame;
import fr.inria.contraintes.biocham.BiochamModel;
import fr.inria.contraintes.biocham.BiochamModelElement;
import fr.inria.contraintes.biocham.customComponents.CustomOperatorLabel;
import fr.inria.contraintes.biocham.customComponents.GradientPanel;
import fr.inria.contraintes.biocham.customComponents.InfoToolTip;
import fr.inria.contraintes.biocham.customComponents.UniversalScrollPane;
import fr.inria.contraintes.biocham.modelData.InitialStateModel;
import fr.inria.contraintes.biocham.modelData.MacrosModel;
import fr.inria.contraintes.biocham.modelData.ParamTableInitConc;
import fr.inria.contraintes.biocham.modelData.ParamTableMacros;
import fr.inria.contraintes.biocham.modelData.ParamTableParameters;
import fr.inria.contraintes.biocham.modelData.ParametersModel;
import fr.inria.contraintes.biocham.utils.Icons;
import fr.inria.contraintes.biocham.utils.Utils;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.ComponentOrientation;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GradientPaint;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.GridLayout;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextArea;
import javax.swing.JToolTip;
import javax.swing.Scrollable;
import javax.swing.SwingConstants;
import javax.swing.ToolTipManager;
import javax.swing.border.TitledBorder;



public class DialogAddSpecification extends JDialog{
	

	   private BiochamModelElement modelElement=null;	   
	   JTextArea formula=null;	
	   JPanel panel=null;
	   String specification;
	   GradientPanel tPanel;
	   static HashMap<String,String> ctlMap=new HashMap<String,String>();
	   static HashMap<String,String> ltlMap=new HashMap<String,String>();   
	   static HashMap<String,String> rulesMap=new HashMap<String,String>();   
	   static HashMap<String,String> kineticsMap=new HashMap<String,String>();
	   static HashMap<String,String> macrosMap=new HashMap<String,String>();
	   String queryVersion=null;
	   String from;
	   BiochamModel model;
       JFrame parentFrame;
      
	   public DialogAddSpecification(String queryversion,JFrame parent,BiochamModelElement element,String n,JPanel pan) {
	      super (parent, true);	   
	      parentFrame=parent;
	      queryVersion=queryversion;
	      modelElement=element;
	      panel=pan;
	      initComponents(n);
	      setResizable(true);
	      pack();
	   }
	   public DialogAddSpecification(JFrame parent,BiochamModelElement element,String n,JPanel pan) {
		      super (parent, true);	   
		      
		      parentFrame=parent;
		      modelElement=element;
		      panel=pan;
		      initComponents(n);
		      setResizable(true);
		      pack();
	   }
	   public DialogAddSpecification(JFrame parent,BiochamModel m,String n,JPanel pan) {
		      super (parent, true);	   
		      
		      parentFrame=parent;
		      model=m;
		      panel=pan;
		      initComponents(n);
		      setResizable(true);
		      pack();
	   }

	 	   
	   
	private void initComponents(String n) {
		  // initialize the window
		
		
	      setTitle("Compose");
	      Container contents = getContentPane();
	      contents.setBackground(Utils.backgroundLighter);
	      BorderLayout bLay=new BorderLayout();
	      bLay.setVgap(30);
	      bLay.setHgap(30);	      
	      contents.setLayout(bLay);
	      from=n;
	      boolean rules=false,macros=false, conservationLaws=false, kinetics=false;
	          	  
	    	  
    	  JLabel intro1=new JLabel("");	    	 
    	  contents.add(intro1,BorderLayout.NORTH);   	  
    	  GridLayout gridLay=null;
    	  GradientPanel mainPanel=null;
    	  formula=new JTextArea();
    	  
    	  GradientPanel tfpanel = new GradientPanel();
    	  tfpanel.setLayout(new GridLayout(1,1));
    	  TitledBorder t1 = null; 	  
    	  if(n.contains("CTL")){
    		  t1 = BorderFactory.createTitledBorder("Biocham CTL Query");
    		  if(ctlMap.get("formula")!=null){
        		  formula.setText(ctlMap.get("formula"));
        	  }
    		  
    	  }else if(n.contains("LTL")){
    		  if(ltlMap.get("formula")!=null){
        		  formula.setText(ltlMap.get("formula"));
        	  }
    		  t1 = BorderFactory.createTitledBorder("Biocham LTL Query");
    	  }else if(n.contains("Rules")){
    		  t1 = BorderFactory.createTitledBorder("Biocham Reaction Rule");
    		  rules=true;
    		  if(rulesMap.get("formula")!=null){
        		  formula.setText(rulesMap.get("formula"));
        	  }
    	  }else if(n.contains("Macro")){    		
    		  if(macrosMap.get("formula")!=null){
        		  formula.setText(macrosMap.get("formula"));
        	  }
    		  macros=true;
    	  }else if(n.contains("Conservation")){
    		  conservationLaws=true;
    	  }else if(n.contains("Kinetics")){
    		  kinetics=true;
    		  if(kineticsMap.get("formula")!=null){
    			  formula.setText(kineticsMap.get("formula"));
    		  }
    	  }
    	  
    	 
    	  formula.setToolTipText("Click to compose!!!");
    	  formula.setRows(3);
    	  UniversalScrollPane spp=new UniversalScrollPane(formula);
    	  spp.setBackground(Utils.backgroundLighter);
    	  tfpanel.add(spp,BorderLayout.CENTER);
    	  tfpanel.setBorder(t1);
    	  gridLay=new GridLayout(2, 1);    	  
    	  gridLay.setHgap(20);
    	  gridLay.setVgap(20);
    	  mainPanel=new GradientPanel();
    	  mainPanel.setLayout(gridLay);
    	    	 
    	  FlowLayout flowLay=new FlowLayout();
    	  flowLay.setAlignment(FlowLayout.LEFT);
    	  
    	  GradientPanel molecules = null,parameters=null,macrosList=null,operPanel=new ScrollableFlowPanel();
    	  operPanel.setLayout(flowLay);
    	  operPanel.applyComponentOrientation(ComponentOrientation.LEFT_TO_RIGHT);
    	  if(n.contains("CTL")){
    		  operPanel.setBorder(BorderFactory.createTitledBorder("Click on the operators to compose your CTL formula"));
    		  JLabel operators[]=getAllCTLOperators();
    		  for(int i=0;i<operators.length;i++){
    			  operPanel.add(operators[i]);
    		  }
    	  }else if(n.contains("LTL")){    		 
    		  GridLayout gl=new GridLayout(2,1);
    		  tPanel=new GradientPanel();
    		  tPanel.setLayout(gl);
    		  gl.setHgap(30);
        	  gl.setVgap(-30);        	
    		  JLabel operators[]=getAllLTLOperators(queryVersion);
    		  operPanel.setBorder(BorderFactory.createTitledBorder("Click on the operators to compose your LTL formula"));
    		  for(int i=0;i<operators.length;i++){
    			  operPanel.add(operators[i]);
    		  }
    		  tPanel.add(operPanel);
    		  GradientPanel tp=new GradientPanel();
    		  tp.setLayout(new BorderLayout());
    		  String text="<html><font size=2>The QFLTL formula curve_fit can be automatically generated from a given Excel file.<br>See" +
      		  		" <a href=''>template</a></font></html>";
    		  
    		  JLabel b2=new JLabel(text,Icons.icons.get("info.png"+0.35),JLabel.LEADING){
 	    		 public JToolTip createToolTip() {
 	    		        return new InfoToolTip("projectImages/CurveFitAGenerationDataXLS.png");
 	    	 }};    	 
 	    	 b2.setToolTipText("");	  
 	    	ToolTipManager.sharedInstance().setEnabled(false);
 	    	 b2.addMouseListener(new MouseAdapter(){
 	    	    	public void mouseClicked(MouseEvent e) {
 	    	    		if(e.getSource() instanceof JLabel){
 	    	    			 if(ToolTipManager.sharedInstance().isEnabled()){
 	    	    				ToolTipManager.sharedInstance().setEnabled(false);
 	    	    			 }else{ 	    	    				
 	    	    				 ToolTipManager.sharedInstance().setInitialDelay(0);
 	    	    				 ToolTipManager.sharedInstance().setEnabled(true); 	 	    	    				 	    	    			
 	    	    			 }		    	    			
 	    	    		}
 	    	    	}
 	    	    }); 
    		  tp.add(b2,BorderLayout.WEST);
    		  tPanel.add(tp);
    		  
    	  }else{
    		  if(macros){
    			  operPanel.setBorder(BorderFactory.createTitledBorder("Click on the operators to compose a Biocham macro"));
        		  JLabel operators[]=getAllMacroOperators();
        		  for(int i=0;i<operators.length;i++){
        			  operPanel.add(operators[i]);
        		  }    		 
    		  }else if(rules){
    			  operPanel.setBorder(BorderFactory.createTitledBorder("Click on the operators to compose Biocham reaction rule"));
        		  JLabel operators[]=getAllRuleOperators();
        		  for(int i=0;i<operators.length;i++){
        			  operPanel.add(operators[i]);
        		  }
    		  }else if(kinetics){
    			  operPanel.setBorder(BorderFactory.createTitledBorder("Click on the operators to compose reaction kinetics"));
        		  JLabel operators[]=getAllMacroOperators();
        		  for(int i=0;i<operators.length;i++){
        			  operPanel.add(operators[i]);
        		  }    	
    		  }else if(n.contains("Volume") || n.contains("Events")){
    			  operPanel.setBorder(BorderFactory.createTitledBorder("Click on the operators to compose simple kinetics"));
        		  JLabel operators[]=getSimpleKineticsOperators();
        		  for(int i=0;i<operators.length;i++){
        			  operPanel.add(operators[i]);
        		  }  
    		  }else if(n.contains("Condition")){
    			  operPanel.setBorder(BorderFactory.createTitledBorder("Click on the operators to compose event condition"));
        		  JLabel operators[]=getConditionOperators();
        		  for(int i=0;i<operators.length;i++){
        			  operPanel.add(operators[i]);
        		  }  
    		  }
    	  }
		  molecules=new ScrollableFlowPanel();
		  molecules.setLayout(flowLay);
		  molecules.applyComponentOrientation(ComponentOrientation.LEFT_TO_RIGHT);
		  molecules.setBorder(BorderFactory.createTitledBorder("Molecules"));
		  JLabel molLabels[]=getAllMolecules(n);	
		  for(int i=0;i<molLabels.length;i++){		
			  molecules.add(molLabels[i]);
		  }
		  if(rules || macros || kinetics || n.contains("LTL") || n.contains("Volume") || n.contains("Events") || n.contains("Condition")){
    		  
    		  parameters=new ScrollableFlowPanel();
    		  parameters.setLayout(flowLay);    		  
    		  parameters.applyComponentOrientation(ComponentOrientation.LEFT_TO_RIGHT);
    		  parameters.setBorder(BorderFactory.createTitledBorder("Parameters"));
    		  JLabel paramLabels[]=getAllParameters(null);
    		  for(int i=0;i<paramLabels.length;i++){
    			  parameters.add(paramLabels[i]);
    		  }
    		  
    		  macrosList=new ScrollableFlowPanel();
    		  macrosList.setLayout(flowLay);    		  
    		  macrosList.applyComponentOrientation(ComponentOrientation.LEFT_TO_RIGHT);
    		  macrosList.setBorder(BorderFactory.createTitledBorder("Macros"));
    		  JLabel macroLabels[]=getAllMacros(null);
    		  for(int i=0;i<macroLabels.length;i++){
    			  macrosList.add(macroLabels[i]);
    		  }
		  }
    	  
    	  operPanel.applyComponentOrientation(ComponentOrientation.LEFT_TO_RIGHT);
    	  if(rules || macros || kinetics || n.contains("LTL") || n.contains("Volume") || n.contains("Events") || n.contains("Condition")){
    		  
    		  GradientPanel top=new GradientPanel();
    		  top.setLayout(new GridLayout(1,2));
    		  GradientPanel topLeft=new GradientPanel();
    		  topLeft.setLayout(new GridLayout(3,1));
    		  
    		  UniversalScrollPane sp=new UniversalScrollPane(molecules);
    		  sp.setBackground(Utils.backgroundLighter);
    		  sp.setBorder(null);
    		  topLeft.add(sp);
    		  
    		  sp=new UniversalScrollPane(parameters);
    		  sp.setBackground(Utils.backgroundLighter);
    		  sp.setBorder(null);
    		  topLeft.add(sp);
    		 
    		  sp=new UniversalScrollPane(macrosList);
    		  sp.setBackground(Utils.backgroundLighter);
    		  sp.setBorder(null);
    		  topLeft.add(sp);
    		  
    		  top.add(tfpanel);
    		  top.add(topLeft);    		  
    		  mainPanel.add(top);   
    		  sp=new UniversalScrollPane(operPanel);
    		  sp.setBackground(Utils.backgroundLighter);
    		  sp.setBorder(null);
    		  mainPanel.add(sp);
    		  sp=null;
    		  
    	  }else if(conservationLaws){
    		  GradientPanel top=new GradientPanel();
    		  top.setLayout(new GridLayout(1,1));
    		  GradientPanel topLeft=new GradientPanel();
    		  topLeft.setLayout(new GridLayout(2,1));
    		  UniversalScrollPane sp=new UniversalScrollPane(molecules);
    		  sp.setBackground(Utils.backgroundLighter);
    		  sp.setBorder(null);
    		  topLeft.add(sp);
    		  topLeft.add(new JLabel("<html><i>Example: {CycE-?} or [A-A,2*A]  </i></html>"));
    		  top.add(tfpanel);//FORMULA
    		  sp=null;	  
    		  mainPanel.add(top);       		
    		  mainPanel.add(topLeft);
    	  }else{
    		  GradientPanel top=new GradientPanel();
    		  top.setLayout(new GridLayout(1,2));
    		  GradientPanel topLeft=new GradientPanel();
    		  topLeft.setLayout(new GridLayout(1,1));
    		  
    		  UniversalScrollPane sp=new UniversalScrollPane(molecules);
    		  sp.setBackground(Utils.backgroundLighter);
    		  sp.setBorder(null);
    		  topLeft.add(sp);
    		  top.add(tfpanel);
    		  top.add(topLeft);    		  
    		  mainPanel.add(top);    		 
    		  if(n.contains("LTL")){
    			  sp=new UniversalScrollPane(tPanel);
    		  }else{
    			  sp=new UniversalScrollPane(operPanel);
    		  }
    		  sp.setBorder(null);
    		  sp.setBackground(Utils.backgroundLighter);
    		  mainPanel.add(sp);
    		  sp=null;
    		  
    	  }
    	 
    	  contents.add(mainPanel,BorderLayout.CENTER);
    	  
    	  JPanel buttons=new JPanel();	    	 	
    	  buttons.setBackground(Utils.backgroundLighter);
    	  JButton b1=new JButton("   OK   ");
    	  b1.addActionListener(new ActionListener(){

			public void actionPerformed(ActionEvent e) {
								
				setFormula(formula.getText());	
				if(from.contains("CTL")){
					ctlMap.put("formula", formula.getText());
				}else if(from.contains("LTL")){
					ltlMap.put("formula", formula.getText());
				}else if(from.contains("Rules")){
					rulesMap.put("formula", formula.getText());
				}else if(from.contains("Kinetics")){
					kineticsMap.put("formula",formula.getText());
				}else if(from.contains("Macro")){
					macrosMap.put("formula",formula.getText());
				}
				setVisible(false);
			    dispose();
			    
				
			}});
    	  JButton b2=new JButton("Cancel");
    	  b2.addActionListener(new ActionListener(){

  			public void actionPerformed(ActionEvent e) {
  								
  				setFormula(null);				
  				setVisible(false);
  			    dispose();
  			}});
    	  
    	  buttons.setLayout(new BoxLayout(buttons, BoxLayout.LINE_AXIS));
    	  buttons.setBorder(BorderFactory.createEmptyBorder(0, 10, 10, 10));
    	  buttons.add(Box.createHorizontalGlue());
    	  buttons.add(b1);
    	  buttons.add(Box.createRigidArea(new Dimension(10, 0)));
    	  buttons.add(b2);
    	  contents.add(buttons,BorderLayout.SOUTH);
	    	
	      
	      JFrame frame=parentFrame;
	    
	      if(!frame.isShowing()){
	    	 try{
	    		  frame.setVisible(true);
	    	 } catch(Exception e){}
	      }else{
	    	  frame=BiochamMainFrame.frame;
	      }
	      Point pos = frame.getLocationOnScreen();
	      setLocation(pos.x+frame.getSize().width/2-185,pos.y+frame.getSize().height/2-140);
	     
	      if(rules){
	    	  setSize(new Dimension(750, 600));	    	  
	      }else{	      
	    	  setSize(new Dimension(500, 500));	    	  
	      }
	      setResizable(true);
	      setLocationRelativeTo(frame);
	      setVisible(true);	   
	}


	private JLabel[] getAllMacros(String n) {
		
		ParamTableMacros pms;
		
		if(modelElement==null){
			pms=(ParamTableMacros)model.getMacros().getParamTable();
		}else{
			pms=(ParamTableMacros)modelElement.getModel().getMacros().getParamTable();
		}
		MacrosModel molsModel=pms.getMacrosModel();
		int len=molsModel.getMacros().size();		
		JLabel operators[]=new CustomOperatorLabel[len];			
		Iterator it = molsModel.getMacros().entrySet().iterator();
		int i=0;
	    while(it.hasNext()){
		      Map.Entry<String,String> me = (Map.Entry)it.next();
		      operators[i]=new CustomOperatorLabel(n,me.getValue(),me.getKey(),"macro",formula);  
		      i++;
	    }
	    it=null;
		return operators;
	}
	

	private JLabel[] getAllParameters(String n) {
		
		ParamTableParameters pms;
		
		if(modelElement==null){
			pms=(ParamTableParameters)model.getParameters().getParamTable();
		}else{
			pms=(ParamTableParameters)modelElement.getModel().getParameters().getParamTable();
		}
		ParametersModel molsModel=pms.getParametersModel();
		int len=molsModel.getParameters().size();		
		JLabel operators[]=new CustomOperatorLabel[len];			
		Iterator it = molsModel.getParameters().entrySet().iterator();
		int i=0;
	    while(it.hasNext()){
		      Map.Entry<String,String> me = (Map.Entry)it.next();
		      operators[i]=new CustomOperatorLabel(n,me.getValue(),me.getKey(),"parameter",formula);  
		      i++;
	    }		
	    it=null;
		return operators;
	}

	private JLabel[] getAllMolecules(String n) {
			
		ParamTableInitConc ic;
		if(modelElement==null){		
			ic=(ParamTableInitConc)model.getInitConditions().getParamTable();
		}else{			
			ic=(ParamTableInitConc)modelElement.getModel().getInitConditions().getParamTable();
		}
		InitialStateModel molsModel=ic.getInitStateModel();
		int len=molsModel.getInitStates().size();		
		//System.out.println("size="+len);
		JLabel operators[]=new CustomOperatorLabel[len];			
		Iterator it = molsModel.getInitStates().entrySet().iterator();
		int i=0;
	    while(it.hasNext()){
		      Map.Entry<String,String> me = (Map.Entry)it.next();
		     // System.out.println("key="+me.getKey()+",value="+me.getValue());
		      operators[i]=new CustomOperatorLabel(n,me.getValue(),me.getKey(),"molecule",formula);  
		      i++;
	    }
	    it=null;
		return operators;
	}

	private JLabel[] getAllRuleOperators() {
		HashMap<String,String> all=Utils.getAllRuleOperators();
		JLabel operators[]=new CustomOperatorLabel[all.size()];		
		int i=0;
		for (Iterator it = all.entrySet().iterator(); it.hasNext();) {
		     Map.Entry entry = (Map.Entry)it.next();
		     operators[i]=new CustomOperatorLabel((String)entry.getKey(),(String)entry.getValue(),formula,"rule");
		     i++;		     
		}		
		return operators;
	}
	//Utils.getSimpleKineticsOperators();
	private JLabel[] getSimpleKineticsOperators() {
		HashMap<String,String> all=Utils.getSimpleKineticsOperators();
		JLabel operators[]=new CustomOperatorLabel[all.size()];		
		int i=0;
		for (Iterator it = all.entrySet().iterator(); it.hasNext();) {
		     Map.Entry entry = (Map.Entry)it.next();
		     operators[i]=new CustomOperatorLabel((String)entry.getKey(),(String)entry.getValue(),formula);
		     i++;		     
		}		
		return operators;
	}
	
	private JLabel[] getConditionOperators() {
		HashMap<String,String> all=Utils.getSimpleKineticsOperators();
		all.putAll(Utils.getAllConditionOperators());
		JLabel operators[]=new CustomOperatorLabel[all.size()];		
		int i=0;
		for (Iterator it = all.entrySet().iterator(); it.hasNext();) {
		     Map.Entry entry = (Map.Entry)it.next();
		     operators[i]=new CustomOperatorLabel((String)entry.getKey(),(String)entry.getValue(),formula);
		     i++;		     
		}		
		return operators;
	}
	
	private JLabel[] getAllMacroOperators() {
		HashMap<String,String> all=Utils.getAllMacroOperators();
		JLabel operators[]=new CustomOperatorLabel[all.size()];		
		int i=0;
		for (Iterator it = all.entrySet().iterator(); it.hasNext();) {
		     Map.Entry entry = (Map.Entry)it.next();
		     operators[i]=new CustomOperatorLabel((String)entry.getKey(),(String)entry.getValue(),formula);
		     i++;		     
		}		
		return operators;
	}

	private JLabel[] getAllLTLOperators(String queryVersion) {
		
		JLabel operators[]=null;
		
		HashMap<String,String> all=Utils.getLtlOperators();
		if(queryVersion!=null){
			
			operators=new CustomOperatorLabel[all.size()];			
			int i=0;
			for (Iterator it = all.entrySet().iterator(); it.hasNext();) {
			     Map.Entry entry = (Map.Entry)it.next();
			     if(((String)entry.getKey()).contains("curve_fit")){				
			    	 operators[i]=new CustomOperatorLabel((String)entry.getKey(),(String)entry.getValue(),modelElement,formula);						
			     }else{
					operators[i]=new CustomOperatorLabel((String)entry.getKey(),(String)entry.getValue(),formula);
			     }			     
			     i++;			     
			}			
		}else{
			operators=new CustomOperatorLabel[all.size()-2];
			int i=0;
			for (Iterator it = all.entrySet().iterator(); it.hasNext();) {
			     Map.Entry entry = (Map.Entry)it.next();
			     if(!((String)entry.getKey()).contains("curve_fit")){				
			    	 operators[i]=new CustomOperatorLabel((String)entry.getKey(),(String)entry.getValue(),formula);
			    	 i++;
			     }		 			     
			}
		}
		return operators;
	}	
	private JLabel[] getAllCTLOperators() {
	
		HashMap<String,String> all=Utils.getCtlOperators();
		JLabel operators[]=new CustomOperatorLabel[all.size()];
		int i=0;
		for (Iterator it = all.entrySet().iterator(); it.hasNext();) {
		     Map.Entry entry = (Map.Entry)it.next();
		     operators[i]=new CustomOperatorLabel((String)entry.getKey(),(String)entry.getValue(),formula);
		     i++;
		     
		}
		return operators;
	}
	
	public String getFormula(){
		return specification;
	}
	
	public void setFormula(String f){
		specification=f;
	}

	
	static public class ScrollableFlowPanel extends GradientPanel implements Scrollable {
		public ScrollableFlowPanel(){
			//super();
			super.setBackground(Utils.backgroundLighter);
			
			//this.getRootPane().setBackground(Utils.backgroundColor);
			//this.getParent().setBackground(Utils.backgroundColor);
		}
		public void setBounds( int x, int y, int width, int height ) {
			super.setBounds( x, y, getParent().getWidth(), height );
			this.getParent().setBackground(Utils.backgroundLighter);
		}
 
		public Dimension getPreferredSize() {
			return new Dimension( getWidth(), getPreferredHeight() );
		}
 
		public Dimension getPreferredScrollableViewportSize() {
			return super.getPreferredSize();
		}
 
		public int getScrollableUnitIncrement( Rectangle visibleRect, int orientation, int direction ) {
			this.getParent().setBackground(Utils.backgroundLighter);
			int hundredth = ( orientation ==  SwingConstants.VERTICAL
					? getParent().getHeight() : getParent().getWidth() ) / 100;
			return ( hundredth == 0 ? 1 : hundredth ); 
		}
 
		public int getScrollableBlockIncrement( Rectangle visibleRect, int orientation, int direction ) {
			this.getParent().setBackground(Utils.backgroundLighter);
			return orientation == SwingConstants.VERTICAL ? getParent().getHeight() : getParent().getWidth();
		}
 
		public boolean getScrollableTracksViewportWidth() {
			return true;
		}
 
		public boolean getScrollableTracksViewportHeight() {
			return false;
		}
 
		private int getPreferredHeight() {
			int rv = 0;
			for ( int k = 0, count = getComponentCount(); k < count; k++ ) {
				Component comp = getComponent( k );
				Rectangle r = comp.getBounds();
				int height = r.y + r.height;
				if ( height > rv )
					rv = height;
			}
			rv += ( (FlowLayout) getLayout() ).getVgap();
			return rv;
		}
		@Override
		public void paintComponent(Graphics g){
			
			if(!isOpaque()){
				super.paintComponent(g);
				return;
			}
			
			Graphics2D g2d=(Graphics2D)g;
			int w= getWidth();
			int h= getHeight();
			GradientPaint gp=new GradientPaint(0,0,getBackground().brighter(),0,h,getBackground());
			g2d.setPaint(gp);
			g2d.fillRect(0,0,w,h);
			
			setOpaque(false);
			super.paintComponent(g);
			setOpaque(true);
		}
	}

}
