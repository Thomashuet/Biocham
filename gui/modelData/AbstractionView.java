package fr.inria.contraintes.biocham.modelData;


import org.jdesktop.swingx.MultiSplitLayout;
import org.jdesktop.swingx.MultiSplitPane;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import fr.inria.contraintes.biocham.BiochamMainFrame;
import fr.inria.contraintes.biocham.BiochamModel;
import fr.inria.contraintes.biocham.customComponents.DnDTabbedPane;
import fr.inria.contraintes.biocham.customComponents.UniversalScrollPane;
import fr.inria.contraintes.biocham.customComponents.CustomPlaf.MyPlafTabbedPane;
import fr.inria.contraintes.biocham.dialogs.InputDialog;
import fr.inria.contraintes.biocham.utils.Utils;
import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JSplitPane;
import javax.swing.JTabbedPane;
import javax.swing.JTextArea;



public class AbstractionView extends JSplitPane implements IView{

	
	
	BiochamModel biochamModel;
	ModelReductions reductionPanel;
	InfluenceGraph igPanel;
	DimensionsOfParameters dimOfParamsPanel;
	ProteinFunctions proteinFuncPanel;
	LocationNeighborhood neighPanel;
	MultiSplitPane multiSplitPane;
	public DnDTabbedPane tabbedPane;		
	AbstractionView thisInstance;
	ParametersPanel[] panels;
	JFrame parentFrame;
	JPanel outputPanel;
	JTextArea tarea;
	static String output="";
	public AbstractionView(JFrame parent,BiochamModel m){
	
		
		biochamModel=m;
		parentFrame=parent;
		tabbedPane = new DnDTabbedPane();
		tabbedPane.setUI(new MyPlafTabbedPane());
		tabbedPane.setOpaque(true);
		tabbedPane.setName("tabbedPane");		
		tabbedPane.setBackground(Utils.backgroundColor);
		tabbedPane.setFocusable(false);
		tabbedPane.setTabLayoutPolicy(JTabbedPane.SCROLL_TAB_LAYOUT);
		outputPanel=new JPanel();			 
		outputPanel.setBorder(BorderFactory.createTitledBorder(BorderFactory.createLineBorder(Color.black),"Output"));
		outputPanel.setLayout(new BorderLayout());	
		outputPanel.setBackground(Utils.backgroundColor);		
		outputPanel.setName("Abstractions Output");
		tarea=new JTextArea();
		tarea.setText(output);		
		tarea.setSelectionColor(Color.WHITE);
		tarea.setBackground(Utils.backgroundColor);
		tarea.setEditable(false);
		outputPanel.add(tarea,BorderLayout.CENTER);
		tabbedPane.add(outputPanel,"Abstractions Output");
		StringBuffer buf=new StringBuffer();
		buf.append("(COLUMN ");
		buf.append("ModelReductions ");	
		buf.append("DimensionsOfParameters ");
		buf.append("InfluenceGraph ");
		buf.append("ProteinFunctions ");
		buf.append("LocationNeighborhood ");
		buf.append(")");
		panels=new ParametersPanel[5];
		
		reductionPanel=new ModelReductions();
		panels[0]=reductionPanel;
		
		dimOfParamsPanel=new DimensionsOfParameters();
		panels[1]=dimOfParamsPanel;
		
		igPanel=new InfluenceGraph();
		panels[2]=igPanel;
		
		proteinFuncPanel=new ProteinFunctions();
		panels[3]=this.proteinFuncPanel;
		
		neighPanel=new LocationNeighborhood();
		panels[4]=neighPanel;
		
		MultiSplitLayout.Node modelRoot = MultiSplitLayout.parseModel(buf.toString());
		multiSplitPane = new MultiSplitPane();
		multiSplitPane.setDividerSize(3);
		multiSplitPane.setContinuousLayout(true);	
		multiSplitPane.getMultiSplitLayout().setModel(modelRoot);	
		
		for(int i=0;i<panels.length;i++){
			multiSplitPane.add(panels[i],panels[i].toString());	
		}
		
			
		super.setOrientation(JSplitPane.HORIZONTAL_SPLIT);
		super.setLeftComponent(new UniversalScrollPane(multiSplitPane));
		super.setRightComponent(new UniversalScrollPane(tabbedPane));
		
		setResizeWeight(0.5);  
		setDividerLocation(400);
		biochamModel.getModelViews().add(this);		
		biochamModel.getAbstractionModel().getViews().add(this);
		thisInstance=this;
	}
	public void appendOutput(String text){
		for(int i=0;i<biochamModel.getAbstractionModel().getViews().size();i++){
			biochamModel.getAbstractionModel().getViews().get(i).getTarea().append("\n");
			biochamModel.getAbstractionModel().getViews().get(i).getTarea().append(text);
			output=biochamModel.getAbstractionModel().getViews().get(i).getTarea().getText();
		}
		
	}
	
	public void showOutput(){
		int ind=tabbedPane.getTabIndexFromTitle("Abstractions Output");
		if(ind>=0){
			tabbedPane.setSelectedIndex(ind);
		}else{
			tabbedPane.add(outputPanel,"Abstractions Output");
			tabbedPane.setSelectedIndex(tabbedPane.getTabIndexFromTitle("Abstractions Output"));
		}
		
	}
	class ModelReductions extends ParametersPanel{

		public ModelReductions() {
			super("ModelReductions");
			setLayout(new BoxLayout(this,BoxLayout.Y_AXIS));
			add(Box.createRigidArea(new Dimension(10, 10)));
			ParametersPanel species=new ParametersPanel("Species");						
			species.setLayout(new FlowLayout(FlowLayout.LEFT));
			add(species);
			ParametersPanel reactions=new ParametersPanel("Reactions");
			reactions.setLayout(new FlowLayout(FlowLayout.LEFT));
			add(reactions);
			ParametersPanel searchReductions=new ParametersPanel("Search Reductions");			
			searchReductions.setLayout(new FlowLayout(FlowLayout.LEFT));
			add(searchReductions);
			add(Box.createRigidArea(new Dimension(10, 20)));
			
			
			JButton mergeSpecies=new JButton("Merge Species");
			mergeSpecies.addActionListener(new ActionListener(){
				public void actionPerformed(ActionEvent e) {
					InputDialog id=new InputDialog(parentFrame,"Merge Species", ((ParamTableMolecules)biochamModel.getMolecules().getParamTable()).getMoleculesModel().getMolecules(), "Object patterns", "Molecule:");
					if(id.getMolecule()!=null && id.getSetOfObjectPatterns()!=null){
						biochamModel.sendToBiochamFromAbstractions("smerge("+id.getSetOfObjectPatterns()+","+id.getMolecule()+").\n",thisInstance);						
					}
					id=null;
			}});			
			JButton deleteSpecies=new JButton("Delete Species");
			deleteSpecies.addActionListener(new ActionListener(){
				public void actionPerformed(ActionEvent e) {
					InputDialog id=new InputDialog(parentFrame,"Delete Species",null,"Object patterns", null);
					if(id.getSetOfObjectPatterns()!=null){
						biochamModel.sendToBiochamFromAbstractions("sdelete("+id.getSetOfObjectPatterns()+").\n",thisInstance);
					}
					id=null;
			}});
			species.add(mergeSpecies);
			species.add(deleteSpecies);
			JButton mergeReactions=new JButton("Merge Reactions");
			mergeReactions.addActionListener(new ActionListener(){
				public void actionPerformed(ActionEvent e) {
					InputDialog id=new InputDialog(parentFrame,"Merge Reactions",null,"Reaction patterns", null);
					if(id.getSetOfObjectPatterns()!=null){
						biochamModel.sendToBiochamFromAbstractions("rmerge("+id.getSetOfObjectPatterns()+").\n",thisInstance);
					}
					id=null;
			}});
			JButton deleteReactions=new JButton("Delete Reactions");
			deleteReactions.addActionListener(new ActionListener(){
				public void actionPerformed(ActionEvent e) {
					InputDialog id=new InputDialog(parentFrame,"Delete Reactions",null,"Reaction patterns", null);
					if(id.getSetOfObjectPatterns()!=null){
						biochamModel.sendToBiochamFromAbstractions("rdelete("+id.getSetOfObjectPatterns()+").\n",thisInstance);
					}
					id=null;
			}});
			reactions.add(mergeReactions);
			reactions.add(deleteReactions);
			
			JButton searchOneReduction=new JButton("One Reduction");
			searchOneReduction.addActionListener(new ActionListener(){
				public void actionPerformed(ActionEvent e) {
					InputDialog id=new InputDialog(parentFrame,"One Reduction",null,"File1:","File2:");
					if(id.getSetOfObjectPatterns()!=null && id.getMolecule()!=null){
						if(id.isMergeOnly()){
							biochamModel.sendToBiochamFromAbstractions("search_mreduction(\'"+id.getSetOfObjectPatterns()+"\',\'"+id.getMolecule()+"\').\n",thisInstance);
						}else{
							biochamModel.sendToBiochamFromAbstractions("search_reduction(\'"+id.getSetOfObjectPatterns()+"\',\'"+id.getMolecule()+"\').\n",thisInstance);	
						}						
					}
					id=null;
			}});
			JButton searchAllReduction=new JButton("All Reductions");
			searchAllReduction.addActionListener(new ActionListener(){
				public void actionPerformed(ActionEvent e) {
					InputDialog id=new InputDialog(parentFrame,"All Reductions",null,"File1:","File2:");
					if(id.getSetOfObjectPatterns()!=null && id.getMolecule()!=null){
						if(id.isMergeOnly()){
							biochamModel.sendToBiochamFromAbstractions("search_all_mreductions(\'"+id.getSetOfObjectPatterns()+"\',\'"+id.getMolecule()+"\').\n",thisInstance);
						}else{
							biochamModel.sendToBiochamFromAbstractions("search_all_reductions(\'"+id.getSetOfObjectPatterns()+"\',\'"+id.getMolecule()+"\').\n",thisInstance);	
						}
						
					}
					id=null;
			}});
			searchReductions.add(searchOneReduction);
			searchReductions.add(searchAllReduction);
		}
		
	}
	class InfluenceGraph extends ParametersPanel{

		public InfluenceGraph() {
			super("InfluenceGraph");
			setLayout(new BoxLayout(this,BoxLayout.Y_AXIS));
			JButton listInfluences=new JButton("List Influences");
			listInfluences.addActionListener(new ActionListener(){
				public void actionPerformed(ActionEvent e) {
					appendOutput("\n");
					biochamModel.sendToBiochamFromAbstractions("list_influences.\n",thisInstance);	
					showOutput();
			}});
			JButton exportInfluencesDOT=new JButton("Export As DOT");
			exportInfluencesDOT.addActionListener(new ActionListener(){
				public void actionPerformed(ActionEvent e) {
					Utils.fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
		   			Utils.fileChooser.setFileFilter(null);	   
					String rep=Utils.showSaveDialog("",parentFrame,".dot");			
					if (rep!=null) {		               
					   biochamModel.sendToBiocham("export_influences_dot(\'"+rep+"\').\n");
					}
				}});	
			JButton exportInfluencesGINML=new JButton("Export As GINML");
			exportInfluencesGINML.addActionListener(new ActionListener(){
				public void actionPerformed(ActionEvent e) {
					Utils.fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
		   			Utils.fileChooser.setFileFilter(null);	   
					String rep=Utils.showSaveDialog("",parentFrame,".dot");			
					if (rep!=null) {		               
					   biochamModel.sendToBiocham("export_influences_ginml(\'"+rep+"\').\n");
					}
				}});	
			JButton drawInfluences=new JButton("Draw");
			drawInfluences.addActionListener(new ActionListener(){
				public void actionPerformed(ActionEvent e) {
					biochamModel.sendToBiochamFromAbstractions("draw_influences.\n",thisInstance);
			}});
			add(Box.createRigidArea(new Dimension(0,10)));
			add(drawInfluences);			
			add(Box.createRigidArea(new Dimension(0,5)));
			add(listInfluences);			
			add(Box.createRigidArea(new Dimension(0,5)));
			add(exportInfluencesDOT);		
			add(Box.createRigidArea(new Dimension(0,5)));
			add(exportInfluencesGINML);		
			add(Box.createRigidArea(new Dimension(0,10)));
		}
		
	}
	class DimensionsOfParameters extends ParametersPanel{
		
		public DimensionsOfParameters() {
			super("DimensionsOfParameters");
			setLayout(new BoxLayout(this,BoxLayout.Y_AXIS));
			JButton listDims=new JButton("List");
			listDims.addActionListener(new ActionListener(){
				public void actionPerformed(ActionEvent e) {
					appendOutput("\n");
					biochamModel.sendToBiochamFromAbstractions("list_dimensions.\n",thisInstance);	
					showOutput();
			}});
			JButton setDim=new JButton("Set Dimension");
			setDim.addActionListener(new ActionListener(){
				public void actionPerformed(ActionEvent e) {
					InputDialog id=new InputDialog(parentFrame,"Set Dimension",((ParamTableParameters)biochamModel.getParameters().getParamTable()).getParametersModel().getParametersNames(),"Dimension:","Parameter");
					if(id.getMolecule()!=null && id.getSetOfObjectPatterns()!=null){
						biochamModel.sendToBiochamFromAbstractions("set_dimension("+id.getMolecule()+","+id.getSetOfObjectPatterns()+").\n",thisInstance);
					}
					id=null;
					showOutput();
			}});
			add(Box.createRigidArea(new Dimension(0,10)));
			add(listDims);
			add(Box.createRigidArea(new Dimension(0,5)));
			add(setDim);
			add(Box.createRigidArea(new Dimension(0,10)));
		}
		/*JPanel right;
		
		public void refreshDimensionsPanel(){
			int size=((ParamTableParameters)biochamModel.getParameters().getParamTable()).getParametersModel().getParameters().size();		
			if(size>0){
				
				SpringLayout layout=(SpringLayout) right.getLayout();
				int northBefore=20;					
				for(String parent: new TreeSet<String>(((ParamTableParameters)biochamModel.getParameters().getParamTable()).getParametersModel().getParameters().keySet())){				
					
					JFormattedTextField tf=new JFormattedTextField();
					tf.setColumns(10);
					tf.setName(parent);
					tf.setValue("-1");
					tf.setHorizontalAlignment(JTextField.LEFT);
					tf.setEditable(false);					
					JLabel l = new JLabel(parent);
					l.setName(parent);	
					l.setLabelFor(tf);					
					right.add(l);
					right.add(tf);	
			    	
			    	
			    	layout.putConstraint(SpringLayout.WEST, l, 5, SpringLayout.WEST, right);
					layout.putConstraint(SpringLayout.NORTH, l, northBefore,SpringLayout.NORTH, right);	
					layout.putConstraint(SpringLayout.WEST, tf, 5, SpringLayout.EAST, l);
					layout.putConstraint(SpringLayout.NORTH, tf, northBefore,SpringLayout.NORTH, right);				
					
					 
					Spring maxSpring = Spring.constant(10);	
					int ccnt=right.getComponentCount();
					int s=ccnt/2;
			    	for (int i=0;i<s;i++)
			    		maxSpring = Spring.max(maxSpring, Spring.sum(layout.getConstraints(right.getComponent(2*i)).getWidth(),Spring.constant(10)));
			    	for (int i=0;i<s;i++)
			    		layout.putConstraint(SpringLayout.WEST, right.getComponent(2*i+1), maxSpring, SpringLayout.WEST, right);
			       			
			    	maxSpring=null;
			    	northBefore+=30;
				}	
				right.setPreferredSize(new Dimension(50,northBefore+60));		
				this.revalidate();
			}	
		}
		public DimensionsOfParameters() {
			super("DimensionsOfParameters");
			JPanel left=new JPanel();			
			left.setBackground(Utils.backgroundColor);
			left.setLayout(new BoxLayout(left,BoxLayout.Y_AXIS));						
			JButton listDims=new JButton("List");
			listDims.addActionListener(new ActionListener(){
				public void actionPerformed(ActionEvent e) {
					biochamModel.sendToBiochamFromPopup("list_dimensions.\n",thisInstance);
			}});
			JButton setDim=new JButton("Set Dimension");
			setDim.addActionListener(new ActionListener(){
				public void actionPerformed(ActionEvent e) {
					InputDialog id=new InputDialog(parentFrame,"Set Dimension",((ParamTableParameters)biochamModel.getParameters().getParamTable()).getParametersModel().getParametersNames(),"Dimension:","Parameter");
					if(id.getMolecule()!=null && id.getSetOfObjectPatterns()!=null){
						biochamModel.sendToBiochamFromPopup("set_dimension("+id.getMolecule()+","+id.getSetOfObjectPatterns()+").\n",thisInstance);
					}
					id=null;					
			}});
			left.add(Box.createRigidArea(new Dimension(0,10)));
			left.add(listDims);
			left.add(Box.createRigidArea(new Dimension(0,5)));
			left.add(setDim);
			left.add(Box.createRigidArea(new Dimension(0,10)));
			right=new JPanel(new SpringLayout());
			right.setBackground(Utils.backgroundColor);

			
			
		    setLayout(new BorderLayout());
		    add(left,BorderLayout.WEST);
		    add(right,BorderLayout.CENTER);
		  
			
		}*/
	}
	class ProteinFunctions extends ParametersPanel{

		public ProteinFunctions() {
			super("ProteinFunctions");
			setLayout(new FlowLayout(FlowLayout.LEFT));
			JButton listFuncs=new JButton("List");
			listFuncs.addActionListener(new ActionListener(){
				public void actionPerformed(ActionEvent e) {
					appendOutput("\n");
					biochamModel.sendToBiochamFromAbstractions("list_functions.\n",thisInstance);	
					showOutput();
			}});
			add(listFuncs);
		}
		
	}
	class LocationNeighborhood extends ParametersPanel{

		public LocationNeighborhood() {
			super("LocationNeighborhood");
			setLayout(new BoxLayout(this,BoxLayout.Y_AXIS));
			JButton listNR=new JButton("List Neighborhood Relations");
			listNR.addActionListener(new ActionListener(){
				public void actionPerformed(ActionEvent e) {
					appendOutput("\n");
					biochamModel.sendToBiochamFromAbstractions("list_neighborhood.\n",thisInstance);					
					showOutput();
			}});
			JButton exportNR=new JButton("Export");
			exportNR.addActionListener(new ActionListener(){
				public void actionPerformed(ActionEvent e) {
					Utils.fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
		   			Utils.fileChooser.setFileFilter(null);	   
					String rep=Utils.showSaveDialog("",parentFrame,".dot");			
					if (rep!=null) {		               
					   biochamModel.sendToBiocham("export_neighborhood_dot(\'"+rep+"\').\n");
					}					
			}});
			
			JButton drawNR=new JButton("Draw");
			drawNR.addActionListener(new ActionListener(){
				public void actionPerformed(ActionEvent e) {
					biochamModel.sendToBiochamFromAbstractions("draw_neighborhood.\n",thisInstance);
			}});
			add(Box.createRigidArea(new Dimension(0,10)));
			add(drawNR);			
			add(Box.createRigidArea(new Dimension(0,5)));
			add(listNR);			
			add(Box.createRigidArea(new Dimension(0,5)));
			add(exportNR);		
			add(Box.createRigidArea(new Dimension(0,10)));
			
		}
		
	}
	public DnDTabbedPane getTabbedPane() {
		return tabbedPane;
	}
	public void setTabbedPane(DnDTabbedPane tabbedPane) {
		
		this.tabbedPane = tabbedPane;
	}
	public void refresh() {
		// TODO Auto-generated method stub
		
	}
	
	public String toString(){
		return "Abstractions";
	}
	public BiochamModel getBiochamModel() {
		return biochamModel;
	}
	public void setBiochamModel(BiochamModel biochamModel) {
		this.biochamModel = biochamModel;
	}
	public ModelReductions getReductionPanel() {
		return reductionPanel;
	}
	public void setReductionPanel(ModelReductions reductionPanel) {
		this.reductionPanel = reductionPanel;
	}
	public InfluenceGraph getIgPanel() {
		return igPanel;
	}
	public void setIgPanel(InfluenceGraph igPanel) {
		this.igPanel = igPanel;
	}
	public DimensionsOfParameters getDimOfParamsPanel() {
		return dimOfParamsPanel;
	}
	public void setDimOfParamsPanel(DimensionsOfParameters dimOfParamsPanel) {
		this.dimOfParamsPanel = dimOfParamsPanel;
	}
	public ProteinFunctions getProteinFuncPanel() {
		return proteinFuncPanel;
	}
	public void setProteinFuncPanel(ProteinFunctions proteinFuncPanel) {
		this.proteinFuncPanel = proteinFuncPanel;
	}
	public LocationNeighborhood getNeighPanel() {
		return neighPanel;
	}
	public void setNeighPanel(LocationNeighborhood neighPanel) {
		this.neighPanel = neighPanel;
	}
	public MultiSplitPane getMultiSplitPane() {
		return multiSplitPane;
	}
	public void setMultiSplitPane(MultiSplitPane multiSplitPane) {
		this.multiSplitPane = multiSplitPane;
	}
	public JFrame getParentFrame() {
		return parentFrame;
	}
	public String getOutput() {
		output=tarea.getText();
		return output;
	}
	public void setOutput(String output) {
		this.output = output;
		tarea.setText(output);
	}
	public JPanel getOutputPanel() {
		return outputPanel;
	}
	public JTextArea getTarea() {
		return tarea;
	}
}
