package fr.inria.contraintes.biocham.graphicalEditor;


import org.jgraph.event.GraphModelEvent;
import org.jgraph.event.GraphModelListener;

import fr.inria.contraintes.biocham.BiochamModel;
import java.awt.BorderLayout;
import java.awt.Container;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.beans.PropertyVetoException;
import javax.swing.BorderFactory;
import javax.swing.JCheckBoxMenuItem;
import javax.swing.JDesktopPane;
import javax.swing.JInternalFrame;
import javax.swing.JLabel;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTabbedPane;
import javax.swing.JToolBar;
import javax.swing.SwingUtilities;

import fr.inria.contraintes.biocham.customComponents.CustomLayoutAKDock;
import fr.inria.contraintes.biocham.customComponents.CustomPlaf;
import fr.inria.contraintes.biocham.customComponents.DnDTabbedPane;
import fr.inria.contraintes.biocham.customComponents.CustomPlaf.MyPlafTabbedPane;
import fr.inria.contraintes.biocham.menus.CustomColorMenu;
import fr.inria.contraintes.biocham.utils.Icons;
import fr.inria.contraintes.biocham.utils.Utils;


public class BiochamGraphEditorDesktop extends JDesktopPane {

	
	
	JInternalFrame frame;
	JPanel biochamGraphPanel;
	JScrollPane graphScrollPanel;
	private BiochamGraph graph;
	GraphEditorToolbar toolBar;
	BiochamGraphEditorDesktop graphDesktop;
	StatusBarGraphListener statusBarListener;
	BiochamStatusBar statusBar;
	BiochamModel biochamModel;
	JCheckBoxMenuItem showInitConc, showKinetics, resetColors;
	//DnDTabbedPane tabbedPane;
	JSplitPane basePanel;
	
	
	public BiochamGraphEditorDesktop(BiochamModel currentModel){
		
		System.setProperty("sun.java2d.d3d", "false");
		graphDesktop=this;
		biochamModel=currentModel;
		Utils.debugMsg("\n\n\n\nBiochamGraphEditorDesktop: BiochamGraphEditorDesktop: Set CurrentModel="+currentModel.getModelName()+"\n\n\n\n");
		/*tabbedPane = new DnDTabbedPane();
		tabbedPane.setUI(new MyPlafTabbedPane());
		tabbedPane.setOpaque(true);
		tabbedPane.setName("tabbedPane");
		tabbedPane.setBackground(Utils.backgroundColor);
		tabbedPane.setFocusable(false);
		tabbedPane.setTabLayoutPolicy(JTabbedPane.SCROLL_TAB_LAYOUT);     
		*/
		frame=new JInternalFrame("Biocham reaction Graphical Editor");	
		statusBarListener = createStatusBar();	
		setGraph(new BiochamGraph(biochamModel,frame));//SampleGraph.createBiochamTemplateGraph();	
		getGraph().setGraphEditorInstance(this);
		getGraph().getModel().addGraphModelListener(new StatusBarGraphListener());
		
		biochamGraphPanel=new JPanel();
		biochamGraphPanel.setLayout(new BorderLayout());
		graphScrollPanel=new JScrollPane(getGraph());
		graphScrollPanel.setAutoscrolls(true);
		biochamGraphPanel.add(graphScrollPanel,BorderLayout.CENTER);		
		frame.setTitle("Biocham reaction Graphical Editor");
		frame.setAutoscrolls(true);
		frame.setClosable(false);
		frame.setMaximizable(false);
		frame.setResizable(false);		
		frame.setFrameIcon(Icons.icons.get("graph.png"+0.3));	
		frame.setLayout(new BorderLayout());		
		addToolbar(frame);
		frame.setJMenuBar(createMenuBar());
		frame.setIconifiable(false);
		
	
		/*basePanel =new JSplitPane();		 	                
		basePanel.setOrientation(JSplitPane.HORIZONTAL_SPLIT);
		basePanel.setLeftComponent(biochamGraphPanel);
		basePanel.setRightComponent(tabbedPane);		
		basePanel.setResizeWeight(0.1);  
		basePanel.setDividerLocation(0.5);
		*/
		frame.add(biochamGraphPanel,BorderLayout.CENTER);
		
		frame.setVisible(true);		
		add(frame);		
			
		biochamGraphPanel.add(statusBarListener, BorderLayout.SOUTH);		
		SwingUtilities.invokeLater(new Runnable(){
		      public void run()
		      {
		        try
		        {
		          frame.setIcon(false);		          
		          frame.setMaximum(true);
		        }
		        catch (PropertyVetoException e)
		        {
		          e.printStackTrace();
		        }
		      }
		    });
		  
	}
	
	
	
	
	
	
	
	
	private void addToolbar(JInternalFrame frame){
	
		Container c=frame.getContentPane();
		c.setLayout(new CustomLayoutAKDock());
		int cnt=c.getComponentCount();	
		for(int i=1;i<cnt;i++){
			if(c.getComponent(i) instanceof JToolBar){
				c.remove(i);   
			}
		}
		toolBar =new GraphEditorToolbar(getGraph());
		c.add(toolBar.getFirstToolBar(),CustomLayoutAKDock.NORTH);
		c.add(toolBar.getSecondToolBar(),CustomLayoutAKDock.NORTH);
		c.validate();
		c.repaint();			
	}
	
	
	
	
	
	protected StatusBarGraphListener createStatusBar() {
		return new BiochamStatusBar();
	}
	
	

	private JMenuBar createMenuBar() {
		JMenuBar menuBar=new JMenuBar();
		menuBar.setBackground(Utils.backgroundColor);
		
		JMenu mn=createMenu("ReactionGraph");	
		mn.add(createMenuItem("Export Image"));
		menuBar.add(mn);	
		
		JMenu mnEdit=createMenu("Edit");		
		mnEdit.add(createMenuItem("Undo"));
		mnEdit.add(createMenuItem("Redo"));
		mnEdit.addSeparator();		
		mnEdit.add(createMenuItem("Cut"));
		mnEdit.add(createMenuItem("Copy"));
		mnEdit.add(createMenuItem("Paste"));
		mnEdit.add(createMenuItem("Remove"));
		
		mnEdit.addSeparator();
		mnEdit.add(createMenuItem("Select All"));
		mnEdit.add(createMenuItem("Deselect All"));
		
		mnEdit.addSeparator();
		mnEdit.add(createMenuItem("Clear"));
		
		
		JMenu mnView=createMenu("View");		
		mnView.add(createMenuItem("Zoom In"));
		mnView.add(createMenuItem("Zoom Out"));
		mnView.addSeparator();
		mnView.add(createMenuItem("Move To Front"));
		mnView.add(createMenuItem("Move To Back"));
		mnView.addSeparator();
		showInitConc = new JCheckBoxMenuItem("Show initial concentrations", null,true);
		showInitConc.setBackground(Utils.backgroundColor);
		showInitConc.addItemListener(new ItemListener(){

				public void itemStateChanged(ItemEvent e) {
					if(e.getStateChange()==ItemEvent.SELECTED){
						showInitConc.setSelected(true);
						getGraph().setShowInitialConcentration(true);
					
					}else{						
						showInitConc.setSelected(false);
						getGraph().setShowInitialConcentration(false);
						
					}				
				}});
		
		showInitConc.setSelected(false);	    
		getGraph().setShowInitialConcentration(false);
		mnView.add(showInitConc);
		
		showKinetics = new JCheckBoxMenuItem("Show rection kinetics", null,true);
		showKinetics.setBackground(Utils.backgroundColor);
		showKinetics.addItemListener(new ItemListener(){

				public void itemStateChanged(ItemEvent e) {
					if(e.getStateChange()==ItemEvent.SELECTED){
						showKinetics.setSelected(true);
						getGraph().setShowReactionKinetics(true);
					
					}else{						
						showKinetics.setSelected(false);
						getGraph().setShowReactionKinetics(false);
						
					}				
				}});
		
		showKinetics.setSelected(false);
		getGraph().setShowReactionKinetics(false);
		mnView.add(showKinetics);
				
		CustomColorMenu mnLayout=new CustomColorMenu("Layout");	
		mnLayout.add(createMenuItem("Save Layout"));		
		CustomColorMenu mnLay=new CustomColorMenu("Run Layout");
		mnLay.add(createMenuItem("Open Layout"));	
		CustomColorMenu dotLay=new CustomColorMenu("DOT Layout");		
		dotLay.add(createMenuItem("DOT Top to Bottom"));
		dotLay.add(createMenuItem("DOT Left to Right"));
		mnLay.add(dotLay);
		mnLay.add(createMenuItem("NEATO Layout"));
		mnLay.add(createMenuItem("CIRCO Layout"));
		mnLayout.add(mnLay);						
		JMenu mnHelp=createMenu("Help");
		mnHelp.add(createMenuItem("SBGN Specification"));		
		menuBar.add(mnEdit);
		menuBar.add(mnView);
		menuBar.add(mnLayout);		
		menuBar.add(mnHelp);		
		
		return menuBar;
	}  

	private JMenu createMenu(String string) {
		JMenu mn=new JMenu(string);		
		mn.setBackground(Utils.backgroundColor);
		
		
		return mn;
	}








	private JMenuItem createMenuItem(String string) {
		JMenuItem mn=new JMenuItem(string);
		mn.setBackground(Utils.backgroundColor);
		mn.setActionCommand(string);
		mn.addActionListener(toolBar);
		return mn;
	}



		


	public class StatusBarGraphListener extends JPanel implements GraphModelListener {

		/**
		 * Graph Model change event
		 */
		public void graphChanged(GraphModelEvent e) {
			updateStatusBar();
		}

		protected void updateStatusBar(){
			Runtime runtime = Runtime.getRuntime();
			int freeMemory = (int) (runtime.freeMemory() / 1024);
			int totalMemory = (int) (runtime.totalMemory() / 1024);
			int usedMemory = (totalMemory - freeMemory);
			String str = (usedMemory / 1024) + "/" + (totalMemory / 1024)
					+ "Mb";
			statusBar.getRightSideStatus().setText(str);
			runtime=null;
			freeMemory=0;
			totalMemory=0;
			usedMemory=0;
			str=null;
		}
	}
	
	
	public class BiochamStatusBar extends StatusBarGraphListener {
		/**
		 * 
		 */
		protected JLabel leftSideStatus;

		/**
		 * contains the scale for the current graph
		 */
		public JLabel rightSideStatus;

		/**
		 * Constructor for GPStatusBar.
		 * 
		 */
		public BiochamStatusBar() {
			super();
			// Add this as graph model change listener
			setBackground(Utils.backgroundColor);
			setLayout(new BorderLayout());
			leftSideStatus = new JLabel("Biocham Reaction Graph Editor");
			rightSideStatus =new JLabel("0/0Mb");;
			leftSideStatus.setBorder(BorderFactory.createLoweredBevelBorder());
			rightSideStatus.setBorder(BorderFactory.createLoweredBevelBorder());
			add(leftSideStatus, BorderLayout.CENTER);
			add(rightSideStatus, BorderLayout.EAST);
			statusBar=this;
		}

		protected void updateStatusBar() {
			Runtime runtime = Runtime.getRuntime();
			int freeMemory = (int) (runtime.freeMemory() / 1024);
			int totalMemory = (int) (runtime.totalMemory() / 1024);
			int usedMemory = (totalMemory - freeMemory);
			String str = (usedMemory / 1024) + "/" + (totalMemory / 1024)
					+ "Mb";
			rightSideStatus.setText(str);
			runtime=null;
			freeMemory=0;
			totalMemory=0;
			usedMemory=0;
			str=null;
		}

		/**
		 * @return Returns the leftSideStatus.
		 */
		public JLabel getLeftSideStatus() {
			return leftSideStatus;
		}

		/**
		 * @param leftSideStatus
		 *            The leftSideStatus to set.
		 */
		public void setLeftSideStatus(JLabel leftSideStatus) {
			this.leftSideStatus = leftSideStatus;
		}

		/**
		 * @return Returns the rightSideStatus.
		 */
		public JLabel getRightSideStatus() {
			return rightSideStatus;
		}

		/**
		 * @param rightSideStatus
		 *            The rightSideStatus to set.
		 */
		public void setRightSideStatus(JLabel rightSideStatus) {
			this.rightSideStatus = rightSideStatus;
		}		
		
	}
	
	public void dispose() {
		
		frame.dispose();
		frame=null;
		biochamGraphPanel=null;
		graphScrollPanel=null;
		setGraph(null);
		toolBar=null;
		graphDesktop=null;
		statusBarListener=null;
		statusBar=null;
		biochamModel=null;
		showInitConc=null;
		showKinetics=null;
		resetColors=null;
		
	}
	public BiochamModel getBiochamModel() {
		return biochamModel;
	}
	public void setBiochamModel(BiochamModel biochamModel) {
		this.biochamModel = biochamModel;
	}
	





	public StatusBarGraphListener getStatusBarListener() {
		return statusBarListener;
	}








	public void setGraph(BiochamGraph graph) {
		this.graph = graph;
	}








	public BiochamGraph getGraph() {
		return graph;
	}
	
}
