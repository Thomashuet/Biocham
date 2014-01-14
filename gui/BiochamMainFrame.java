package fr.inria.contraintes.biocham;

import fr.inria.contraintes.biocham.customComponents.CustomLayoutAKDock;
import fr.inria.contraintes.biocham.customComponents.DnDTabbedPane;
import fr.inria.contraintes.biocham.documentation.BiochamDocumentationToolbar;
import fr.inria.contraintes.biocham.utils.BiochamUpdater;
import fr.inria.contraintes.biocham.utils.BrowserLauncher;
import fr.inria.contraintes.biocham.utils.Icons;
import fr.inria.contraintes.biocham.utils.OSXAdapter;
import fr.inria.contraintes.biocham.utils.SwingWorker;
import fr.inria.contraintes.biocham.utils.Utils;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.FileDialog;
import java.awt.Insets;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.util.prefs.Preferences;

import javax.swing.JCheckBoxMenuItem;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JMenuBar;
import javax.swing.JOptionPane;
import javax.swing.JTabbedPane;
import javax.swing.JToolBar;
import javax.swing.UIManager;



/**
 * A class representing the working explorer for the opened Biocham Models.
 * It implements several listeners so it can be user-interactive explorer.
 * Mouse Listener, to answers on right-click on node actions( it popups a node-corresponding menu), left-click on node actions( it shows the content of the clicked node), drag-and-drop node actions( only Biocham Plot Image can be dragged and droped on a special Comparison Window)  
 * 
 * @author Dragana Jovanovska 
 */
public class BiochamMainFrame implements KeyListener{
	
	
	
	 public static JFrame frame;
	 //static GuiInitialization s;
	 static DnDTabbedPane main_screen;
	 static JMenuBar menuBar;
	 static	JToolBar toolBar;
	 public static Preferences prefs;
	 static int ws_index;	
	 public static JCheckBoxMenuItem checkForUpdatesBox=new JCheckBoxMenuItem();
	 static String[] screenNames =new String[]{"Workbench","Documentation"};
	 public static JComponent screens[];
	 public static BiochamDynamicTree tree;
	 private static JComponent currentPanel;
	 private static BiochamDocumentationToolbar docToolbar;
	 static BiochamMainFrame instance;

	 /**
	  *    
	  * 
	  * @param args 
	  */
	 public BiochamMainFrame(final BiochamSplash splash) {
		 
		
		 initLookAndFeel();
		 instance=this;
			 
		 frame = new JFrame("BioCham");		
		 frame.setLayout(new BorderLayout());
		// frame.setAlwaysOnTop(false);
		 frame.addKeyListener(this);
		 frame.setDefaultCloseOperation(JFrame.DO_NOTHING_ON_CLOSE);	     
	     WindowListener l = new WindowAdapter() {
	    	 
		        public void windowClosing(WindowEvent e) {

		        	quit();
		        }
	     }; 
	     frame.addWindowListener(l);
	     prefs = Preferences.userNodeForPackage(this.getClass());	     
	     int xpos = prefs.getInt("xpos", 0);
	     int ypos = prefs.getInt("ypos", 0);
	     frame.setLocation(xpos, ypos);
	     int xsize = prefs.getInt("xsize", 900);
	     int ysize = prefs.getInt("ysize", 750);
	     frame.setSize(xsize, ysize); 
	     Icons.addImage("SSicon.png");
	     frame.setIconImage(Icons.images.get("SSicon.png"));	     
	     SwingWorker sw=new SwingWorker(){
			
			public Object construct() {
				
			
				screens=new JComponent[screenNames.length];
				screens[0]=BiochamWorkbenchPart.getScreen(screenNames[0]);				
				createAllTheScreens();				  
		
				if(Utils.is_OS_MAC){		 
					 Utils.macFileChooser= new FileDialog (BiochamMainFrame.frame);
					 registerForMacOSXEvents();
			
				}				
				initializeFrame();	
				return 1;
			}
			
			public void finished() {			
				
				
				
				splash.setVisible(false);
				splash.dispose();
				frame.setVisible(true);	
				frame.invalidate();
			
			}
		
		};
		sw.start();
	     
	   
		
	     
	 }	 

	 	// Generic registration with the Mac OS X application menu
	    // Checks the platform, then attempts to register with the Apple EAWT
	    // See OSXAdapter.java to see how this is done without directly referencing any Apple APIs
	    private void registerForMacOSXEvents() {	       
	            try {
	                // Generate and register the OSXAdapter, passing it a hash of all the methods we wish to
	                // use as delegates for various com.apple.eawt.ApplicationListener methods
	                OSXAdapter.setQuitHandler(null, getClass().getDeclaredMethod("quit", (Class[])null));
	                OSXAdapter.setAboutHandler(null, getClass().getDeclaredMethod("about", (Class[])null));
	            } catch (Exception e) {
	                System.err.println("Error while loading the OSXAdapter:");
	                e.printStackTrace();
	            }
	       
	    }

	private void initLookAndFeel() {
		
		try{
			
			UIManager.put("Menu.selectionBackground", Utils.menuSelectionBackground);
			UIManager.put("MenuItem.selectionBackground", Utils.menuSelectionBackground);
			UIManager.put("Menu.disabledBackground", Color.gray);
			UIManager.put("TextField.selectionBackground", new Color(175,238,238));	
			UIManager.put("TabbedPane.selected", Utils.selectedTabColor);
			UIManager.put("TabbedPane.font", Utils.menuBarFont);
			UIManager.put("TabbedPane.selectedForeground", Color.BLACK);
			UIManager.put("TabbedPane.unselectedForeground", Color.LIGHT_GRAY);
			UIManager.put("TabbedPane.unselectedBackground", Utils.selectedTabColor.darker());
			UIManager.put("TextField.selectionBackground", new Color(175,238,238));	
			UIManager.put("TableHeader.background", Utils.backgroundColor);			
			UIManager.put("ScrollBar.width",15);
			UIManager.put("MenuItem.background",Utils.backgroundColor);
			UIManager.put("Menu.background",Utils.backgroundColor);
			UIManager.put("MenuBar.background", Utils.backgroundColor);
			UIManager.put("JMenuItem.backgroundColor", Utils.backgroundColor);
		
			if(Utils.is_OS_MAC){	
				
				Utils.debugMsg("ITS MAC OS X");		
				System.setProperty("com.apple.mrj.application.apple.menu.about.name", "Biocham");
				System.setProperty("apple.laf.useScreenMenuBar", "true");
				System.setProperty("com.apple.mrj.application.growbox.intrudes","false");			
				System.setProperty("com.apple.mrj.application.live-resize","true");	
			
			
				try {
	                // Generate and register the OSXAdapter, passing it a hash of all the methods we wish to
	                // use as delegates for various com.apple.eawt.ApplicationListener methods
	                OSXAdapter.setQuitHandler(this, getClass().getDeclaredMethod("quit", (Class[])null));
	                OSXAdapter.setAboutHandler(this, getClass().getDeclaredMethod("about", (Class[])null));
	                OSXAdapter.setPreferencesHandler(this, null);
	                OSXAdapter.setFileHandler(this, getClass().getDeclaredMethod("loadBiochamFile", new Class[] { String.class }));
	            } catch (Exception e) {
	                Utils.debugMsg("Error while loading the OSXAdapter:");
	                e.printStackTrace();
	            }
			}
			System.setProperty("sun.swing.enableImprovedDragGesture", "true");
			UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());		
		}catch(Exception e){
			Utils.debugMsg("Unable to load native look and feel");
		}
		
	}




	/**
	 * 
	 * 	 
	 */
	public static void initializeFrame(){
		
				
		showWorkbenchScreen();				
		boolean updates=prefs.getBoolean("checkForUpdates", true);		
		if (updates) {
			
	         BiochamUpdater bu = new BiochamUpdater("http://contraintes.inria.fr/BIOCHAM/BiochamUpdates.xml", frame);
	         bu.start();
		}
		
	}

	
	
	
	public static void showWorkbenchScreen(){
				
		
		clearFrame();		
    	JComponent c=getScreenByName("Workbench");	
    	BiochamMainFrame.frame.setJMenuBar(((WorkbenchArea)c).getMenuBar());
    	BiochamMainFrame.addToolBar(((WorkbenchArea)c).getToolBar());
    	if(BiochamDynamicTree.openedModels.size()>0){
    		BiochamMainFrame.addToolBar(WorkbenchArea.wtb.getSecondToolBar());    		
    	}
	    frame.getContentPane().add(c,BorderLayout.CENTER);
	    c=null;
	   		
	}
	

	public static void showDocumentationScreen(){
		
		
		clearFrame();
		frame.setJMenuBar(null);		
		//frame.removeKeyListener(instance);//.addKeyListener(instance);
		frame.addKeyListener(instance);
		Container c=frame.getContentPane();
		c.setLayout(WorkbenchArea.toolBarLayout);
		int cnt=c.getComponentCount();	
		for(int i=1;i<cnt;i++){
			if(c.getComponent(i) instanceof JToolBar){
				c.remove(i);   
			}
		}
		if(docToolbar==null){
			docToolbar=new BiochamDocumentationToolbar();
		}
		c.add(docToolbar.getTb(),CustomLayoutAKDock.NORTH);
		//c.validate();
		c.repaint();
		//c.addKeyListener(instance);
		//BiochamMainFrame.frame.setJMenuBar(((WorkbenchArea)c).getMenuBar());
    	//BiochamMainFrame.addToolBar(((WorkbenchArea)c).getToolBar());
		//BiochamMainFrame.addToolBar();
    //	frame.setJMenuBar(WelcomeMenuBar.getWelcomeMenuBar());
		frame.getContentPane().add(getScreenByName("Documentation"), BorderLayout.CENTER);	
		
	//	BiochamMainFrame.frame.setAlwaysOnTop(true);
		BiochamMainFrame.frame.setEnabled(true);
		c=null;
	}
	
	
	
	
	
	
	/**
	 * 
	 */
	private static void createAllTheScreens() {
						
		for(int i=1; i<screens.length;i++){
			
			screens[i]=BiochamWorkbenchPart.getScreen(screenNames[i]);	
			
		}
	}
	
	public static JComponent getScreenByName(String name){
				
		for(int i=0;i<screens.length;i++){
			if(screens[i]!=null){
				if(screens[i].toString().equals(name)){
					currentPanel=screens[i];
					return screens[i];
				}
			}
		}
		return null;
	}
	
	
	private static void clearFrame() {
	
		Component[] comps=frame.getContentPane().getComponents();		
		int num=comps.length;
		if(num>0){
			for (int i=0;i<num;i++){					
					comps[i].setVisible(false);
					frame.getContentPane().remove(comps[i]);
			}			
		}
		comps=null;
	}
	
	
	
	public static void addToolBar(JToolBar toolBar){				
		
		Container c=frame.getContentPane();
		c.setLayout(WorkbenchArea.toolBarLayout);
		int cnt=c.getComponentCount();	
		for(int i=1;i<cnt;i++){
			if(c.getComponent(i) instanceof JToolBar){
				c.remove(i);   
			}
		}
		c.add(toolBar,CustomLayoutAKDock.NORTH);
		//c.validate();
		c.repaint();			
	}
	
	
	public static void about(){
		JOptionPane.showMessageDialog(frame,
	            "BIOCHAM (C) 2003-2011 INRIA, France,\n" +
	            "by N. Chabrier-Rivier, F. Fages and S. Soliman.\n" +
	            "http://contraintes.inria.fr/BIOCHAM/\n\n" +
	            "Graphical User Interface\n" +
	            "by D. Jovanovska, S. Soliman and L. Calzone",
	            "About",
	            JOptionPane.INFORMATION_MESSAGE);
	}
	
	public static void aboutNotOSXPlatform() {
		
			
	      JOptionPane.showMessageDialog(frame,
	            "BIOCHAM (C) 2003-2011 INRIA, France,\n" +
	            "by N. Chabrier-Rivier, F. Fages and S. Soliman.\n" +
	            "http://contraintes.inria.fr/BIOCHAM/\n\n" +
	            "Graphical User Interface\n" +
	            "by D. Jovanovska, S. Soliman and L. Calzone",
	            "About",
	            JOptionPane.INFORMATION_MESSAGE);
	}
	
	public static void loadBiochamFile(String path) {
        
        	SwingWorker sw=new SwingWorker(){

				@Override
				public Object construct() {
					tree.treeListener.openBCmodel();
					return null;
				}

				@Override
				public void finished() {
					// TODO Auto-generated method stub
					
				}};
				sw.start(); 
        
    }
	
	
	public static void quit() {
		

		 if (JOptionPane.showConfirmDialog(frame,
	               "Are you sure you want to quit?",
	               "Confirmation",
	               JOptionPane.WARNING_MESSAGE) == JOptionPane.YES_OPTION) {	            
			 try  {
						
				 int siz=BiochamDynamicTree.openedModels.size();
				 for(int i=0;i<siz;i++){
					 BiochamDynamicTree.openedModels.get(i).sendToBiocham("quit.\n");
				 }
				/* Dimension dim = frame.getSize();
		         Preferences prefs = Preferences.userNodeForPackage(BiochamMainFrame.frame.getClass());
		         prefs.putBoolean("checkForUpdates", checkForUpdatesBox.isSelected());
		         prefs.putInt("xsize", dim.width);
		         prefs.putInt("ysize", dim.height);
		         Point pos = frame.getLocationOnScreen();
		         prefs.putInt("xpos", pos.x);
		         prefs.putInt("ypos", pos.y);*/
				 for(int i=0;i<screens.length;i++){
					 
					 screens[i]=null;
				 }				 
		        
			 } catch (Exception e) {
				 Utils.debugMsg(e.getClass()+","+e.getMessage());
			 }			 
	         System.exit(0); 
		 }
	}
	

	
	public static Preferences getPrefs() {
		return prefs;
	}

	public static JComponent getCurrentPanel() {
		
		return currentPanel;
	}

	public static void setCurrentPanel(JComponent p) {
		
		currentPanel=p;
	}

	public void keyPressed(KeyEvent e) {
		
		
		
		if(e.getKeyCode()==KeyEvent.VK_N){
			if(BiochamMainFrame.tree!=null){
				BiochamMainFrame.tree.treeListener.newBCmodel();
			}
		}else if(e.getKeyCode()==KeyEvent.VK_O){
			if(BiochamMainFrame.tree!=null){
				BiochamMainFrame.tree.treeListener.openBCmodel();
			}
		}else if(e.getKeyCode()==KeyEvent.VK_Q){
			BiochamMainFrame.quit();
		}else if(e.getKeyCode()==KeyEvent.VK_L){
			BiochamMainFrame.showDocumentationScreen();
		}else if(e.getKeyCode()==KeyEvent.VK_I){
			
				Thread th=new Thread(new Runnable(){

					public void run() {
						BrowserLauncher.openURL("http://contraintes.inria.fr/BIOCHAM/DOC/manual.html");
					}
				});
				th.start();
				
		}else if(e.getKeyCode()==KeyEvent.VK_A){
			if(Utils.is_OS_MAC){
				BiochamMainFrame.about();
			}else{
				BiochamMainFrame.aboutNotOSXPlatform();
			}
			
		}else if(e.getKeyCode()==KeyEvent.VK_S){
			if(BiochamDynamicTree.currentModel!=null){
				BiochamDynamicTree.currentModel.saveToFile(false);
			}
			
		}else if(e.getKeyCode()==KeyEvent.VK_R){
			
		}else if(e.getKeyCode()==KeyEvent.VK_B || e.getKeyCode()==KeyEvent.VK_BACK_SPACE){
			BiochamMainFrame.showWorkbenchScreen();
		}
		
	}

	public void keyReleased(KeyEvent e) {
		// TODO Auto-generated method stub
		
	}

	public void keyTyped(KeyEvent e) {
	
		if(e.getKeyCode()==KeyEvent.VK_N){
			if(BiochamMainFrame.tree!=null){
				BiochamMainFrame.tree.treeListener.newBCmodel();
			}
		}else if(e.getKeyCode()==KeyEvent.VK_O){
			if(BiochamMainFrame.tree!=null){
				BiochamMainFrame.tree.treeListener.openBCmodel();
			}
		}else if(e.getKeyCode()==KeyEvent.VK_Q){
			BiochamMainFrame.quit();
		}else if(e.getKeyCode()==KeyEvent.VK_L){
			BiochamMainFrame.showDocumentationScreen();
		}else if(e.getKeyCode()==KeyEvent.VK_I){
			
				Thread th=new Thread(new Runnable(){

					public void run() {
						BrowserLauncher.openURL("http://contraintes.inria.fr/BIOCHAM/DOC/manual.html");
					}
				});
				th.start();
				
		}else if(e.getKeyCode()==KeyEvent.VK_A){
			if(Utils.is_OS_MAC){
				BiochamMainFrame.about();
			}else{
				BiochamMainFrame.aboutNotOSXPlatform();
			}
			
		}else if(e.getKeyCode()==KeyEvent.VK_S){
			if(BiochamDynamicTree.currentModel!=null){
				BiochamDynamicTree.currentModel.backupModel();
			}
			
		}else if(e.getKeyCode()==KeyEvent.VK_R){
			
		}else if(e.getKeyCode()==KeyEvent.VK_B || e.getKeyCode()==KeyEvent.VK_BACK_SPACE){
			BiochamMainFrame.showWorkbenchScreen();
		}
		
	}


}
