/* BIOCHAM system http://contraintes.inria.fr/BIOCHAM/
 * Copyright 2003-2008, INRIA, Projet Contraintes
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 *
 * BiochamGUI.java
 * by Sylvain Soliman */

package fr.inria.contraintes.biocham;

import java.io.*;
import java.util.*;
import java.util.prefs.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.event.*;
import java.beans.*;
import java.text.*;

public class BiochamGUI extends WindowAdapter
   implements ActionListener, KeyListener, PropertyChangeListener {
   JFrame frame;
   JSplitPane splitPane;
   JTextField input;
   JTextArea output;
   JScrollPane scrollPane;
   JFileChooser fc;
   JMenuItem openMenuItem;
   JMenuItem addMenuItem;
   JMenuItem openSbmlMenuItem;
   JMenuItem addSbmlMenuItem;
   JMenuItem openTraceMenuItem;
   JMenuItem saveMenuItem;
   JMenuItem expandMenuItem;
   JMenuItem exp_initMenuItem;
   JMenuItem exp_paramMenuItem;
   JMenuItem exp_dotMenuItem;
   JMenuItem exp_smvMenuItem;
   JMenuItem exp_sbmlMenuItem;
   JMenuItem exp_odeMenuItem;
   JMenuItem exp_texMenuItem;
   JMenuItem quitMenuItem;
   JMenuItem numMenuItem;
   JMenuItem numofMenuItem;
   JMenuItem sifMenuItem;
   JMenuItem lpMenuItem;
   JMenuItem contMenuItem;
   JMenuItem plotMenuItem;
   JMenuItem dotMenuItem;
   JMenuItem dataMenuItem;
   JMenuItem aboutMenuItem;
   JMenuItem dummyMenuItem;
   JMenuItem searchMenuItem;
   JMenuItem keepMenuItem;
   JMenuItem showMenuItem;
   JMenuItem helpMenuItem;
   JMenuItem tutMenuItem;
   JMenuItem listMenuItem;
   JMenuItem expandrMenuItem;
   JMenuItem clearMenuItem;
   JMenuItem showkineMenuItem;
   JMenuItem fitXMenuItem;
   JMenuItem fitYMenuItem;
   JMenuItem fitXPMenuItem;
   JMenuItem fitYPMenuItem;
   JCheckBoxMenuItem checkForUpdatesBox;
   JPopupMenu popupMenu;
   PrintStream bcOut;
   BufferedReader bcIn;
   LoggerThread logger;
   Vector<String> historyVector;
   int historyIndex;
   static String[] arguments;
   JPanel parametersPanel, initValuesPanel;
   ParamTable paramTable, initValTable;
   JavaPlot plot;
   DataTable dtable;
   JTabbedPane tabbedPane;
   float lastSimLength;
   static String app_path;

   /**Constructor.*/
   public BiochamGUI() {
      historyVector = new Vector<String>();
      historyIndex = -1;
      parametersPanel = new JPanel();
      paramTable = new ParamTable(this, parametersPanel);
      initValuesPanel = new JPanel();
      initValTable = new ParamTable(this, initValuesPanel);
      // default simulation length
      lastSimLength = 20;
   }

   /**Create the menu bar.*/
   public JMenuBar createMenuBar(Boolean updates) {
      JMenuBar menuBar = new JMenuBar();
      JMenu menu;
      
      //File menu
      menu = new JMenu("File");
      menu.setMnemonic(KeyEvent.VK_F);
      
      openMenuItem = new JMenuItem("Open...",
            createImageIcon("images/SSOpen.png"));
      openMenuItem.setMnemonic(KeyEvent.VK_O);
      openMenuItem.setAccelerator(KeyStroke.getKeyStroke(
               KeyEvent.VK_O, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()));
      openMenuItem.addActionListener(this);
      menu.add(openMenuItem);
      
      addMenuItem = new JMenuItem("Add...",
            createImageIcon("images/SSAdd.png"));
      addMenuItem.setMnemonic(KeyEvent.VK_A);
      addMenuItem.setAccelerator(KeyStroke.getKeyStroke(
               KeyEvent.VK_A, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()));
      addMenuItem.addActionListener(this);
      menu.add(addMenuItem);
      
      openSbmlMenuItem = new JMenuItem("Open SBML...",
            createImageIcon("images/SSnull.png"));
      openSbmlMenuItem.addActionListener(this);
      menu.add(openSbmlMenuItem);
      
      addSbmlMenuItem = new JMenuItem("Add SBML...",
            createImageIcon("images/SSnull.png"));
      addSbmlMenuItem.addActionListener(this);
      menu.add(addSbmlMenuItem);
      
      openTraceMenuItem = new JMenuItem("Open Trace...",
            createImageIcon("images/SSnull.png"));
      openTraceMenuItem.addActionListener(this);
      menu.add(openTraceMenuItem);
      
      menu.addSeparator();
      
      saveMenuItem = new JMenuItem("Save As...",
            createImageIcon("images/SSSave.png"));
      saveMenuItem.setMnemonic(KeyEvent.VK_S);
      saveMenuItem.setAccelerator(KeyStroke.getKeyStroke(
               KeyEvent.VK_S, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()));
      saveMenuItem.addActionListener(this);
      menu.add(saveMenuItem);
      
//      expandMenuItem = new JMenuItem("Save (Expanded) As...",
//            createImageIcon("images/SSnull.png"));
//      expandMenuItem.setMnemonic(KeyEvent.VK_X);
//      expandMenuItem.addActionListener(this);
//      menu.add(expandMenuItem);
//      
      exp_initMenuItem = new JMenuItem("Save Initial cond. as...",
            createImageIcon("images/SSnull.png"));
      exp_initMenuItem.setMnemonic(KeyEvent.VK_I);
      exp_initMenuItem.addActionListener(this);
      menu.add(exp_initMenuItem);
      
      exp_paramMenuItem = new JMenuItem("Save Parameters as...",
            createImageIcon("images/SSnull.png"));
      exp_paramMenuItem.setMnemonic(KeyEvent.VK_P);
      exp_paramMenuItem.addActionListener(this);
      menu.add(exp_paramMenuItem);
      
      menu.addSeparator();
      
      exp_dotMenuItem = new JMenuItem("Export Dot...",
            createImageIcon("images/SSnull.png"));
      exp_dotMenuItem.addActionListener(this);
      menu.add(exp_dotMenuItem);
      
      exp_odeMenuItem = new JMenuItem("Export ODE...",
            createImageIcon("images/SSnull.png"));
      exp_odeMenuItem.addActionListener(this);
      menu.add(exp_odeMenuItem);
      
      exp_texMenuItem = new JMenuItem("Export LaTeX...",
            createImageIcon("images/SSnull.png"));
      exp_texMenuItem.addActionListener(this);
      menu.add(exp_texMenuItem);
      
      exp_sbmlMenuItem = new JMenuItem("Export SBML...",
            createImageIcon("images/SSnull.png"));
      exp_sbmlMenuItem.addActionListener(this);
      menu.add(exp_sbmlMenuItem);
      
      exp_smvMenuItem = new JMenuItem("Export NuSMV...",
            createImageIcon("images/SSnull.png"));
      exp_smvMenuItem.addActionListener(this);
      menu.add(exp_smvMenuItem);
      
//      dummyMenuItem = new JMenuItem("Export Lotos...",
//            createImageIcon("images/SSnull.png"));
//      menu.add(dummyMenuItem);
//      
      menu.addSeparator();
      checkForUpdatesBox = new JCheckBoxMenuItem("Check for updates",
            createImageIcon("images/SSnull.png"),
            updates);
      menu.add(checkForUpdatesBox);
      
      if (System.getProperty("mrj.version") == null) { // not Mac
         menu.addSeparator();
      
         quitMenuItem = new JMenuItem("Quit",
               createImageIcon("images/SSQuit.png"));
         quitMenuItem.setMnemonic(KeyEvent.VK_Q);
         quitMenuItem.setAccelerator(KeyStroke.getKeyStroke(
                  KeyEvent.VK_Q, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()));
         quitMenuItem.addActionListener(this);
         menu.add(quitMenuItem);
      } else { // Mac OS
         //System.setProperty("com.apple.macos.useScreenMenuBar", "true");
         //System.setProperty("com.apple.mrj.application.growbox.intrudes",
         //      "false");
         //System.setProperty("com.apple.mrj.application.apple.menu.about.name",
         //      "Biocham");
      }
      
      menuBar.add(menu);
      
      //Edit Menu
      menu = new JMenu("Edit");
      menu.setMnemonic(KeyEvent.VK_E);
      
      searchMenuItem = new JMenuItem("Find...",
            createImageIcon("images/SSFind.png"));
      searchMenuItem.setMnemonic(KeyEvent.VK_F);
      searchMenuItem.setAccelerator(KeyStroke.getKeyStroke(
               KeyEvent.VK_F, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()));
      searchMenuItem.addActionListener(this);
      menu.add(searchMenuItem);
      
//      dummyMenuItem = new JMenuItem("Graph preferences...",
//            createImageIcon("images/SSnull.png"));
//      dummyMenuItem.setEnabled(false);
//      menu.add(dummyMenuItem);
      
      dummyMenuItem = new JMenuItem("Numerical preferences...",
            createImageIcon("images/SSnull.png"));
      dummyMenuItem.setEnabled(false);
      menu.add(dummyMenuItem);
      
      menuBar.add(menu);
      
      //Rules Menu
      menu = new JMenu("Rules");
      menu.setMnemonic(KeyEvent.VK_R);
      
      listMenuItem = new JMenuItem("List",
            createImageIcon("images/SSnull.png"));
      listMenuItem.setMnemonic(KeyEvent.VK_L);
      listMenuItem.addActionListener(this);
      menu.add(listMenuItem);
      
      expandrMenuItem = new JMenuItem("Expand",
            createImageIcon("images/SSnull.png"));
      expandrMenuItem.setMnemonic(KeyEvent.VK_E);
      expandrMenuItem.addActionListener(this);
      menu.add(expandrMenuItem);
      
      clearMenuItem = new JMenuItem("Clear",
            createImageIcon("images/SSnull.png"));
      clearMenuItem.setMnemonic(KeyEvent.VK_C);
      clearMenuItem.addActionListener(this);
      menu.add(clearMenuItem);
      
      showkineMenuItem = new JMenuItem("Show Kinetics",
            createImageIcon("images/SSnull.png"));
      showkineMenuItem.setMnemonic(KeyEvent.VK_S);
      showkineMenuItem.addActionListener(this);
      menu.add(showkineMenuItem);
      
      menuBar.add(menu);
      
      //Numerics Menu
      menu = new JMenu("Numerics");
      menu.setMnemonic(KeyEvent.VK_N);
      
      numMenuItem = new JMenuItem("Num. Simulation",
            createImageIcon("images/SSNum.png"));
      numMenuItem.setMnemonic(KeyEvent.VK_S);
      numMenuItem.addActionListener(this);
      menu.add(numMenuItem);
      
      numofMenuItem = new JMenuItem("Num. simulation for...",
            createImageIcon("images/SSNum.png"));
      numofMenuItem.setMnemonic(KeyEvent.VK_N);
      numofMenuItem.setAccelerator(KeyStroke.getKeyStroke(
               KeyEvent.VK_N, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()));
      numofMenuItem.addActionListener(this);
      menu.add(numofMenuItem);
      
      contMenuItem = new JMenuItem("ConTinue for...",
            createImageIcon("images/SSnull.png"));
      contMenuItem.setMnemonic(KeyEvent.VK_T);
      contMenuItem.setAccelerator(KeyStroke.getKeyStroke(
               KeyEvent.VK_T, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()));
      contMenuItem.addActionListener(this);
      menu.add(contMenuItem);
      
      sifMenuItem = new JMenuItem("Set Init from trace...",
            createImageIcon("images/SSnull.png"));
      sifMenuItem.setMnemonic(KeyEvent.VK_I);
      sifMenuItem.setAccelerator(KeyStroke.getKeyStroke(
               KeyEvent.VK_I, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()));
      sifMenuItem.addActionListener(this);
      menu.add(sifMenuItem);
      
      lpMenuItem = new JMenuItem("Learn parameters...",
            createImageIcon("images/SSnull.png"));
      lpMenuItem.setMnemonic(KeyEvent.VK_L);
      lpMenuItem.addActionListener(this);
      menu.add(lpMenuItem);
      
      menuBar.add(menu);
      
      //View Menu
      menu = new JMenu("View");
      menu.setMnemonic(KeyEvent.VK_V);
      
      plotMenuItem = new JMenuItem("Simulation Plot",
            createImageIcon("images/SSPlot.png"));
      plotMenuItem.setMnemonic(KeyEvent.VK_P);
      plotMenuItem.setAccelerator(KeyStroke.getKeyStroke(
               KeyEvent.VK_P, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()));
      plotMenuItem.addActionListener(this);
      menu.add(plotMenuItem);
      
      dataMenuItem = new JMenuItem("Simulation Data",
            createImageIcon("images/SSData.png"));
      dataMenuItem.setMnemonic(KeyEvent.VK_D);
      dataMenuItem.setAccelerator(KeyStroke.getKeyStroke(
               KeyEvent.VK_D, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()));
      dataMenuItem.addActionListener(this);
      menu.add(dataMenuItem);
      
      keepMenuItem = new JMenuItem("Keep plot",
            createImageIcon("images/SSnull.png"));
      keepMenuItem.setMnemonic(KeyEvent.VK_K);
      keepMenuItem.addActionListener(this);
      menu.add(keepMenuItem);
      
      showMenuItem = new JMenuItem("Show/hide ...",
            createImageIcon("images/SSnull.png"));
      showMenuItem.setMnemonic(KeyEvent.VK_K);
      showMenuItem.addActionListener(this);
      menu.add(showMenuItem);
      
      fitXMenuItem = new JMenuItem("Fit X",
            createImageIcon("images/SSnull.png"));
      fitXMenuItem.setMnemonic(KeyEvent.VK_X);
      fitXMenuItem.addActionListener(this);
      menu.add(fitXMenuItem);
      
      fitYMenuItem = new JMenuItem("Fit Y",
            createImageIcon("images/SSnull.png"));
      fitYMenuItem.setMnemonic(KeyEvent.VK_Y);
      fitYMenuItem.addActionListener(this);
      menu.add(fitYMenuItem);
      
      dotMenuItem = new JMenuItem("Network Graph",
            createImageIcon("images/SSnull.png"));
      dotMenuItem.setMnemonic(KeyEvent.VK_N);
      //dotMenuItem.setAccelerator(KeyStroke.getKeyStroke(
      //         KeyEvent.VK_P, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()));
      dotMenuItem.addActionListener(this);
      menu.add(dotMenuItem);
      
      menuBar.add(menu);


      //menuBar.add(Box.createHorizontalGlue());
      
      //Help Menu
      menu = new JMenu("Help");
      menu.setMnemonic(KeyEvent.VK_H);
      
      helpMenuItem = new JMenuItem("Browse (local) reference Manual",
            createImageIcon("images/SSnull.png"));
      helpMenuItem.setMnemonic(KeyEvent.VK_M);
      helpMenuItem.addActionListener(this);
      menu.add(helpMenuItem);
      
      tutMenuItem = new JMenuItem("Browse (online) Tutorial",
            createImageIcon("images/SSnull.png"));
      tutMenuItem.setMnemonic(KeyEvent.VK_T);
      tutMenuItem.addActionListener(this);
      menu.add(tutMenuItem);
      
      if (System.getProperty("mrj.version") == null) { // not Mac
         menu.addSeparator();
         
         aboutMenuItem = new JMenuItem("About",
               createImageIcon("images/SSAbout.png"));
         aboutMenuItem.setMnemonic(KeyEvent.VK_A);
         aboutMenuItem.addActionListener(this);
         menu.add(aboutMenuItem);
      } else { // Mac OS
         new BiochamApp(this);
      }
      menuBar.add(menu);

      // Popup menu
      popupMenu = new JPopupMenu();
      
      fitXPMenuItem = new JMenuItem("Fit X");
      fitXPMenuItem.addActionListener(this);
      popupMenu.add(fitXPMenuItem);
      
      fitYPMenuItem = new JMenuItem("Fit Y");
      fitYPMenuItem.addActionListener(this);
      popupMenu.add(fitYPMenuItem);
      
      return menuBar;
   }
      
   /**
    * Create the GUI and show it.
    *
    * For thread safety, this method should be invoked from the
    * event-dispatching thread.
    */
   private static void createAndShowGUI() {
      BiochamGUI gui = new BiochamGUI();
      String biocham;
      
      //Make sure we have nice window decorations.
      //JFrame.setDefaultLookAndFeelDecorated(true);
      //JDialog.setDefaultLookAndFeelDecorated(true);

      //Create and set up the window.
      gui.frame = new JFrame("Biocham");
      gui.frame.setDefaultCloseOperation(JFrame.DO_NOTHING_ON_CLOSE);
      gui.frame.addWindowListener(gui);

      Preferences prefs = Preferences.userNodeForPackage(gui.getClass());
      
      gui.frame.setJMenuBar(gui.createMenuBar(prefs.getBoolean("updates",
                  true)));

      gui.createContentPane();

      if ((biocham = gui.findExecutable()) != null) {
         gui.createPipes(biocham);
      }

      gui.fc = new JFileChooser();
      gui.fc.addChoosableFileFilter(new BiochamFileFilter());
      gui.fc.addChoosableFileFilter(new XmlFileFilter());
      gui.fc.addChoosableFileFilter(new CsvFileFilter());
      
      String dir = prefs.get("directory",null);
      if (dir != null)
         gui.fc.setCurrentDirectory(new File(dir));

      //Display the window.
      int xpos = prefs.getInt("xpos", 0);
      int ypos = prefs.getInt("ypos", 0);
      gui.frame.setLocation(xpos, ypos);
      int xsize = prefs.getInt("xsize", 900);
      int ysize = prefs.getInt("ysize", 750);
      gui.frame.setSize(xsize, ysize);
      gui.splitPane.setDividerLocation(prefs.getInt("xdiv", -1));
      //gui.frame.pack();
      gui.frame.setIconImage(createImage("images/SSicon.png"));
      gui.frame.setVisible(true);
      
      if (gui.bcOut == null) {
         JOptionPane.showMessageDialog(gui.frame,
               "Cannot start the 'biocham' program. Please check your PATH.",
               "Warning",
               JOptionPane.WARNING_MESSAGE);
         try  {
            gui.bcOut.close();
            gui.bcIn.close();
         } catch (Exception e) {}
         System.exit(0); 
      }
      
      if (prefs.getBoolean("updates", true)) {
         BiochamUpdater bu = new BiochamUpdater("http://contraintes.inria.fr/BIOCHAM/BiochamUpdates.xml", gui);
         bu.start();
      }
   }

   /** Create the main content pane.
    *
    * Text area, upper left and lower left panels.
    */
   public void createContentPane() {
      //Create the console Panel
      JPanel consolePanel = new JPanel(new GridBagLayout());
      GridBagConstraints c = new GridBagConstraints();
      
      consolePanel.setOpaque(true);

      c.gridx=0;
      c.gridy=0;
      consolePanel.add(new JLabel("Command: "), c);
      
      input = new JTextField(80);
      input.addActionListener(this);
      // avoid TAB events consumption by FocusTraversal
      // keep them for command completion
      input.setFocusTraversalKeysEnabled(false);
      input.addKeyListener(this);
      
      c.gridx=1;
      c.gridy=0;
      c.fill=GridBagConstraints.HORIZONTAL;
      consolePanel.add(input, c);
      
      //Create a scrolled text area.
      output = new JTextArea(5, 30);
      output.setEditable(false);
      output.setFont(new Font("Monospaced",Font.PLAIN,12));
      scrollPane = new JScrollPane(output);

      //Add the text area to the panel.
      c.gridx=0;
      c.gridy=1;
      c.fill=GridBagConstraints.BOTH;
      c.gridwidth=2;
      c.weightx=1.0;
      c.weighty=1.0;
      consolePanel.add(scrollPane, c);

      //Create a new plotting area
      tabbedPane = new JTabbedPane();
      plot = new JavaPlot(tabbedPane, popupMenu);
      tabbedPane.add("Plot", plot);
      tabbedPane.addMouseListener(plot);
      //Focus bug when removing tabs...
      tabbedPane.setFocusable(false);
      tabbedPane.setTabLayoutPolicy(JTabbedPane.SCROLL_TAB_LAYOUT);
      
      JSplitPane rightSplitPane = new JSplitPane(
            JSplitPane.VERTICAL_SPLIT,tabbedPane,consolePanel);
      
      rightSplitPane.setResizeWeight(0.7);

      //Create left panel for parameters and initial values
      
      //Create upper left panel
      parametersPanel.setBorder(BorderFactory.createTitledBorder(
                BorderFactory.createLineBorder(Color.black),
                "Parameters"));
      
      parametersPanel.setLayout(new SpringLayout());
      
      //Create lower left panel
      initValuesPanel.setBorder(BorderFactory.createTitledBorder(
               BorderFactory.createLineBorder(Color.black),
               "Initial Concentrations"));
      
      initValuesPanel.setLayout(new SpringLayout());
      
      // Put left pane together
      JSplitPane leftSplitPane = new JSplitPane(
            JSplitPane.VERTICAL_SPLIT,
            new JScrollPane(parametersPanel,
               JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
               JScrollPane.HORIZONTAL_SCROLLBAR_NEVER),
            new JScrollPane(initValuesPanel,
               JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
               JScrollPane.HORIZONTAL_SCROLLBAR_NEVER));
      
      leftSplitPane.setResizeWeight(0.5);
      leftSplitPane.setMinimumSize(new Dimension(120,0));

      //Put everything together
      
      splitPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT,
                              leftSplitPane, rightSplitPane);
      
      splitPane.setResizeWeight(0.1);
      frame.setContentPane(splitPane);
   }

   /**Try to find the biocham executable in the path.*/
   public String findExecutable() {

      File f = new File(System.getProperty("user.dir"));      
      String ps = File.pathSeparator;
      String one_path = app_path+ps+new File(f.getParent()).getParent()+ps+System.getenv("PATH");
      if (one_path == null)
         one_path = "";
                  
      /* JOptionPane.showMessageDialog(frame,
            "*"+app_path+"--"+one_path,
            "Path",
            JOptionPane.WARNING_MESSAGE); */

      String[] path = one_path.split(ps);
      for (int i=0;i<path.length;++i) {
         String bc = path[i]+File.separator+"biocham";
         if (System.getProperty("os.name").toLowerCase().indexOf("windows") != -1)
            bc = bc+".exe";   // Windows case
         if ((new File(bc)).exists()) {
            return bc;
         }
      }
      return null;
   }
   
   /**Puts the given extension for the file chooser.*/
   public void updateFc(String ext) {
      File f = fc.getSelectedFile();
      if (f != null) {
         String s = f.getPath();
         if (!s.endsWith(ext)) {
            int i = s.lastIndexOf('.');
            if (i <= s.lastIndexOf(File.separatorChar))
               s = s.concat(ext);
            else
               s = s.substring(0,i).concat(ext);

            fc.setSelectedFile(new File(s));
         }
      }
      javax.swing.filechooser.FileFilter [] filters = fc.getChoosableFileFilters();
      int i=filters.length-1;
      while(! filters[i].accept(new File(ext))) {
         i--;
      }
      fc.setFileFilter(filters[i]);
   }
   
   /**Checks is a file exists and if so if the user wants to overwrite it.*/
   public boolean checkConfirmOvw(File f,String ext) {
      File file;
      if (f.getPath().lastIndexOf('.') < 0)
         file = new File(f.getPath().concat(ext));
      else
         file = f;
      return (! file.exists() || JOptionPane.showConfirmDialog(frame,
               "File ("+file+") exists, overwrite?",
               "Confirmation",
               JOptionPane.WARNING_MESSAGE) == JOptionPane.YES_OPTION);
   }
      
   /**Pipe opening with the BIOCHAM executable.
    *
    * try to launch BIOCHAM and set up the pipes and LoggerThread.
    */
   public void createPipes(String biocham) {
      try {
         String[] cmd = {biocham, "--have_gui", app_path};
		 ProcessBuilder pb = new ProcessBuilder(cmd);
		 //pb.redirectOutput(ProcessBuilder.Redirect.INHERIT);
		 Process proc = pb.start();
         // Process proc = Runtime.getRuntime().exec(biocham + " --have_gui \"" + app_path + "\"");
         // autoflush
         bcOut = new PrintStream(proc.getOutputStream(),true);
         bcIn = new BufferedReader(new InputStreamReader(proc.getInputStream()));
      } catch (Exception e) {}
      logger = new LoggerThread(this);
      logger.start();
      
      for (int i=0;i<arguments.length;++i)
         if (arguments[i].endsWith(".bc")) {
            String s = "add_biocham('" + arguments[i] + "').\n";
            bcOut.print(s);
            output.append(s);
         }
   }
   
   /**Action dispatcher.
    *
    * sends to BIOCHAM actions triggered by menus.
    */
   public void actionPerformed(ActionEvent e) {
      Object source = e.getSource();
      
      if (source == openMenuItem) {
         updateFc(".bc");
         int returnVal = fc.showOpenDialog(frame);

         if (returnVal == JFileChooser.APPROVE_OPTION) {
            File file = fc.getSelectedFile();
            String s = "load_biocham('"+ file.getPath() +"').\n";
            bcOut.print(s);
            output.append(s);
         }
      }
      
      if (source == addMenuItem) {
         updateFc(".bc");
         int returnVal = fc.showOpenDialog(frame);

         if (returnVal == JFileChooser.APPROVE_OPTION) {
            File file = fc.getSelectedFile();
            String s = "add_biocham('"+ file.getPath() +"').\n";
            bcOut.print(s);
            output.append(s);
         }
      }
      
      if (source == openSbmlMenuItem) {
         updateFc(".xml");
         int returnVal = fc.showOpenDialog(frame);

         if (returnVal == JFileChooser.APPROVE_OPTION) {
            File file = fc.getSelectedFile();
            String s = "load_sbml('"+ file.getPath() +"').\n";
            bcOut.print(s);
            output.append(s);
         }
      }
      
      if (source == addSbmlMenuItem) {
         updateFc(".xml");
         int returnVal = fc.showOpenDialog(frame);

         if (returnVal == JFileChooser.APPROVE_OPTION) {
            File file = fc.getSelectedFile();
            String s = "add_sbml('"+ file.getPath() +"').\n";
            bcOut.print(s);
            output.append(s);
         }
      }
      
      if (source == openTraceMenuItem) {
         updateFc(".csv");
         int returnVal = fc.showOpenDialog(frame);

         if (returnVal == JFileChooser.APPROVE_OPTION) {
            File file = fc.getSelectedFile();
            String s = "load_trace('"+ file.getPath() +"').\n";
            bcOut.print(s);
            output.append(s);
         }
      }
      
      if (source == saveMenuItem) {
         updateFc(".bc");
         int returnVal = fc.showSaveDialog(frame);

         if (returnVal == JFileChooser.APPROVE_OPTION) {
            File file = fc.getSelectedFile();
            if (checkConfirmOvw(file,".bc")) {
               String s = "export_biocham('"+ file.getPath() +"').\n";
               bcOut.print(s);
               output.append(s);
            }
         }
      }
      
      if (source == expandMenuItem) {
         updateFc(".bc");
         int returnVal = fc.showSaveDialog(frame);

         if (returnVal == JFileChooser.APPROVE_OPTION) {
            File file = fc.getSelectedFile();
            if (checkConfirmOvw(file,".bc")) {
               String s = "expand_biocham('"+ file.getPath() +"').\n";
               bcOut.print(s);
               output.append(s);
            }
         }
      }
      
      if (source == exp_initMenuItem) {
         updateFc(".bc");
         int returnVal = fc.showSaveDialog(frame);

         if (returnVal == JFileChooser.APPROVE_OPTION) {
            File file = fc.getSelectedFile();
            if (checkConfirmOvw(file,".bc")) {
               String s = "export_init('"+ file.getPath() +"').\n";
               bcOut.print(s);
               output.append(s);
            }
         }
      }
      
      if (source == exp_paramMenuItem) {
         updateFc(".bc");
         int returnVal = fc.showSaveDialog(frame);

         if (returnVal == JFileChooser.APPROVE_OPTION) {
            File file = fc.getSelectedFile();
            if (checkConfirmOvw(file,".bc")) {
               String s = "export_param('"+ file.getPath() +"').\n";
               bcOut.print(s);
               output.append(s);
            }
         }
      }
      
      if (source == exp_dotMenuItem) {
         updateFc(".dot");
         int returnVal = fc.showSaveDialog(frame);

         if (returnVal == JFileChooser.APPROVE_OPTION) {
            File file = fc.getSelectedFile();
            if (checkConfirmOvw(file,".dot")) {
               String s = "export_dot('"+ file.getPath() +"').\n";
               bcOut.print(s);
               output.append(s);
            }
         }
      }
      
      if (source == exp_smvMenuItem) {
         updateFc(".smv");
         int returnVal = fc.showSaveDialog(frame);

         if (returnVal == JFileChooser.APPROVE_OPTION) {
            File file = fc.getSelectedFile();
            if (checkConfirmOvw(file,".smv")) {
               String s = "export_nusmv('"+ file.getPath() +"').\n";
               bcOut.print(s);
               output.append(s);
            }
         }
      }
      
      if (source == exp_sbmlMenuItem) {
         updateFc(".xml");
         int returnVal = fc.showSaveDialog(frame);

         if (returnVal == JFileChooser.APPROVE_OPTION) {
            File file = fc.getSelectedFile();
            if (checkConfirmOvw(file,".xml")) {
               String s = "export_sbml('"+ file.getPath() +"').\n";
               bcOut.print(s);
               output.append(s);
            }
         }
      }
      
      if (source == exp_odeMenuItem) {
         updateFc(".ode");
         int returnVal = fc.showSaveDialog(frame);

         if (returnVal == JFileChooser.APPROVE_OPTION) {
            File file = fc.getSelectedFile();
            if (checkConfirmOvw(file,".ode")) {
               String s = "export_ode('"+ file.getPath() +"').\n";
               bcOut.print(s);
               output.append(s);
            }
         }
      }
      
      if (source == exp_texMenuItem) {
         updateFc(".tex");
         int returnVal = fc.showSaveDialog(frame);

         if (returnVal == JFileChooser.APPROVE_OPTION) {
            File file = fc.getSelectedFile();
            if (checkConfirmOvw(file,".tex")) {
               String s = "export_ode_latex('"+ file.getPath() +"').\n";
               bcOut.print(s);
               output.append(s);
            }
         }
      }
      
      if (source == searchMenuItem) {
         // check if a DataTable is active
         if (dtable !=null && tabbedPane.getSelectedComponent().getClass() == dtable.getClass()) {
            DataTable dt = (DataTable) tabbedPane.getSelectedComponent();
            // get starting point
            int y = dt.table.getSelectedRow();
            int x = dt.table.getSelectedColumn();
            Float f = (Float) dt.table.getValueAt(y,x);
            // get end row
            int max_y = dt.table.getRowCount();
            // get value to search for
            String result = (String) JOptionPane.showInputDialog(frame,
                  "Value to find in the currently selected column of data",
                  "Find value",
                  JOptionPane.QUESTION_MESSAGE,
                  null,
                  null,
                  f.toString());
            if (result != null)
               try {
                  Float g = Float.parseFloat(result);
                  Float sign = g-f;
                  // continue until we go over the searched value
                  // in the right direction (sign)
                  while (y<max_y && ((Float) dt.table.getValueAt(y,x)-g)*sign<0)
                     y++;
                  if (y<max_y) {
                     // select the answer cell
                     dt.table.setRowSelectionInterval(y,y);
                     // put it into view
                     JViewport viewport = dt.getViewport();
                     Rectangle viewrect = viewport.getViewRect();
                     Rectangle rect = dt.table.getCellRect(y, x, true);
                     rect.setLocation(rect.x-viewrect.x, rect.y-viewrect.y);
                     viewport.scrollRectToVisible(rect);
                  }
                  else {
                     Toolkit.getDefaultToolkit().beep();
                  }
               } catch  (Exception excp) {
                  JOptionPane.showMessageDialog(frame,
                        "Invalid value: "+result,
                        "Invalid value",
                        JOptionPane.WARNING_MESSAGE);
               }
         }
      }
      
      if (source == numMenuItem) {
         String s = "make_absent_not_present.\nnumerical_simulation.\nplot.\n";
         bcOut.print(s);
         output.append(s);
      }
      
      if (source == plotMenuItem) {
         String s = "plot.\n";
         bcOut.print(s);
         output.append(s);
      }
      
      if (source == keepMenuItem) {
         String s = "keep_plot.\n";
         bcOut.print(s);
         output.append(s);
      }
      
      if (source == showMenuItem) {
         bcOut.print("show_hide.\n");
         output.append("show_hide.\n");
      }
      
      if (source == fitXMenuItem || source == fitXPMenuItem) {
         JavaPlot p = (JavaPlot) tabbedPane.getSelectedComponent();
         if (p != null) {
            p.setXmin("*");
            p.setXmax("*");
            p.repaint();
         }
      }
      
      if (source == fitYMenuItem || source == fitYPMenuItem) {
         JavaPlot p = (JavaPlot) tabbedPane.getSelectedComponent();
         if (p != null) {
            p.setYmin("*");
            p.setYmax("*");
            p.repaint();
         }
      }
      
      if (source == dotMenuItem) {
         String s = "dot.\n";
         bcOut.print(s);
         output.append(s);
      }
      
      if (source == listMenuItem) {
         String s = "list_rules.\n";
         bcOut.print(s);
         output.append(s);
      }
      
      if (source == expandrMenuItem) {
         String s = "expand_rules.\n";
         bcOut.print(s);
         output.append(s);
      }
      
      if (source == clearMenuItem) {
         if (JOptionPane.showConfirmDialog(frame,
               "Are you sure you want to erase all rules?",
               "Confirmation",
               JOptionPane.WARNING_MESSAGE) == JOptionPane.YES_OPTION) {
            String s = "clear_rules.\n";
            bcOut.print(s);
            output.append(s);
         }
      }
      
      if (source == showkineMenuItem) {
         String s = "show_kinetics.\n";
         bcOut.print(s);
         output.append(s);
      }
      
      if (source == dataMenuItem) {
         JavaPlot p = null;
         int i = tabbedPane.getComponentCount()-1;
         while (p == null && i>=0)
            if (tabbedPane.getComponent(i).getClass() == plot.getClass())
               p = (JavaPlot) tabbedPane.getComponent(i);
            else
               i = i-1;
         if (p != null && p.ready) {
            dtable = new DataTable(p);
            tabbedPane.add(dtable, "Data");
         }
      }
      
      if (source == numofMenuItem) {
         String result = (String) JOptionPane.showInputDialog(frame,
                  "Simulation length",
                  "Numerical Simulation of given length",
                  JOptionPane.QUESTION_MESSAGE,
                  null,
                  null,
                  lastSimLength);
         if (result != null)
            try {
               lastSimLength = Float.parseFloat(result);
               String s = "make_absent_not_present.\nnumerical_simulation("+lastSimLength+").\nplot.\n";
               bcOut.print(s);
               output.append(s);
            } catch  (Exception excp) {
               JOptionPane.showMessageDialog(frame,
                     "Invalid value: "+result,
                     "Invalid value",
                     JOptionPane.WARNING_MESSAGE);
            }
      }
      
      if (source == sifMenuItem) {
         String result = (String) JOptionPane.showInputDialog(frame,
                  "Time point",
                  "Set initial state from trace",
                  JOptionPane.QUESTION_MESSAGE,
                  null,
                  null,
                  null);
         if (result != null)
            try {
               String s = "set_init_from_trace("+Float.parseFloat(result)+").\n";
               bcOut.print(s);
               output.append(s);
            } catch  (Exception excp) {
               JOptionPane.showMessageDialog(frame,
                     "Invalid value: "+result,
                     "Invalid value",
                     JOptionPane.WARNING_MESSAGE);
            }
      }
      
      if (source == lpMenuItem) {
         LearnParamDialog dialog = new LearnParamDialog(frame, true,
               paramTable, lastSimLength);
         dialog.setPreferredSize(new Dimension(370,280));
         Point pos = frame.getLocationOnScreen();
         dialog.setLocation(pos.x+frame.getSize().width/2-185,
               pos.y+frame.getSize().height/2-140);
         dialog.setResizable(false);
         dialog.setVisible(true);
         String result = dialog.getResult();
         if (result != null) {
            bcOut.print(result);
            output.append(result);
         }
      }
      
      if (source == contMenuItem) {
         String result = (String) JOptionPane.showInputDialog(frame,
                  "Simulation length",
                  "Increase Numerical Simulation by given length",
                  JOptionPane.QUESTION_MESSAGE,
                  null,
                  null,
                  lastSimLength);
         if (result != null)
            try {
               Float contSimLength = Float.parseFloat(result);
               lastSimLength += contSimLength;
               String s = "continue("+contSimLength+").\nplot.\n";
               bcOut.print(s);
               output.append(s);
            } catch  (Exception excp) {
               JOptionPane.showMessageDialog(frame,
                     "Invalid value: "+result,
                     "Invalid value",
                     JOptionPane.WARNING_MESSAGE);
            }
      }
      
      if (source == quitMenuItem) {
         quit();
      }

      if (source == aboutMenuItem) {
         about();
      }

      if (source == helpMenuItem) {
         browseUrl(app_path+File.separator+"DOC"+File.separator+"manual.html");
      }

      if (source == tutMenuItem) {
         browseUrl("http://contraintes.inria.fr/~calzone/tutorial/tutorial_dream.htm");
      }

      if (source == input) {
         String s = input.getText();
         bcOut.println(s);
         if (historyVector.size() > 31)
            historyVector.removeElementAt(0);
         historyVector.add(s);
         output.append(s + "\n");
         input.setText(null);
         historyIndex = -1;
      }

      output.setCaretPosition(output.getDocument().getLength());
   }

   /**Window listener.
    *
    * unused...
    */
   public void windowClosing(WindowEvent e) {
      quit();
   }
   
   /**Key listener.
    *
    * unused...
    */
   public void keyTyped(KeyEvent e) {
   }
   
   /**Key listener.
    *
    * unused...
    */
   public void keyReleased(KeyEvent e) {
   }

   /**Key listener.
    *
    * listen for up/down keys to update history of commands.
    */
   public void keyPressed(KeyEvent e) {
      // History
      if (e.getKeyCode() == KeyEvent.VK_UP && !historyVector.isEmpty()) {
         if (historyIndex < 0)
            historyIndex = historyVector.size()-1;
         else
            if (historyIndex > 0)
               historyIndex--;
         input.setText(historyVector.elementAt(historyIndex));
      }
      if (e.getKeyCode() == KeyEvent.VK_DOWN && historyIndex >= 0) {
         if (historyIndex < historyVector.size()-1) {
            historyIndex++;
            input.setText(historyVector.elementAt(historyIndex));
         }
         else {
            historyIndex=-1;
            input.setText(null);
         }
      }
      // Command completion
      if (e.getKeyCode() == KeyEvent.VK_TAB) {
         input.setText(BiochamCommands.getContinuation(input.getText()));
      }
   }

   /**Property Change listener.
    *
    * update parameter values w.r.t. corresponding text field.
    */
   public void propertyChange (PropertyChangeEvent e) {
      int i=0;
      JFormattedTextField j = (JFormattedTextField) e.getSource();
      Container c = j.getParent();
      while(c.getComponent(i) != j && i< c.getComponentCount())
         i++;
      String s1;
      if (c == parametersPanel) {
         s1 = "parameter";
      } else {
         s1 = "present";
      }
      String source = ((JLabel) c.getComponent(i-1)).getText();
      String value = (String) j.getValue();
      Boolean ok = false;
      if (c != parametersPanel && value.matches("[a-zA-Z]\\w*"))
         // present(molecule, name)
         ok = true;
      else try {
         value = Float.parseFloat(value)+"";
         ok = true;
      } catch (NumberFormatException nfe) {}

      //send it to BIOCHAM
      String s;
      
      if (ok) {
         s = s1 + "(" + source + ", " + value + ").\n";
         bcOut.print(s);
         output.append(s);
         output.setCaretPosition(output.getDocument().getLength());
      } else
         if (c == parametersPanel)
            // get current value
            bcOut.print("parameter("+source+").\n");
         else
            bcOut.print("show_initial_state.\n");
   }

   /**Broswer opening (for help)*/
   public void browseUrl(String url) {
      String os = System.getProperty("os.name");
      String cmd = null;
      try
      {
         if (os != null && os.startsWith("Windows")) {
            cmd = "rundll32 url.dll,FileProtocolHandler " + url;
            Process p = Runtime.getRuntime().exec(cmd);
         }
         else if (System.getProperty("mrj.version") != null) { // Mac
            Runtime.getRuntime().exec(new String[] {"open", url});
            // com.apple.eio.FileManager.openURL(url);
         }
         else {
            try
            {
               // works under Debian Linux (mime-support package)
               // cmd = "run-mailcap text/html:" + url;
               // but doesn't handle distant URLs
               cmd = "sensible-browser " + url;
               Process p = Runtime.getRuntime().exec(cmd);
            }
            catch(IOException x)
            {
               // Command failed, start up the browser
               cmd = "mozilla "  + url;
               Process p = Runtime.getRuntime().exec(cmd);
            }
         }
      }
      catch(IOException x)
      {
         // couldn't exec browser
         System.err.println("Could not invoke browser, command=" + cmd);
         System.err.println("Caught: " + x);
      }
      //System.err.println(cmd);
   }

   /**Clean quitting.*/
   public void quit() {
      if (JOptionPane.showConfirmDialog(frame,
               "Are you sure you want to quit?",
               "Confirmation",
               JOptionPane.WARNING_MESSAGE) == JOptionPane.YES_OPTION) {
            bcOut.println("quit.");
            exit();
            System.exit(0); 
      }
   }
   
   public void exit() {
      try  {
         bcOut.close();
         bcIn.close();
         Dimension dim = frame.getSize();
         Preferences prefs = Preferences.userNodeForPackage(this.getClass());
         prefs.putBoolean("updates", checkForUpdatesBox.getState());
         prefs.putInt("xsize", dim.width);
         prefs.putInt("ysize", dim.height);
         Point pos = frame.getLocationOnScreen();
         prefs.putInt("xpos", pos.x);
         prefs.putInt("ypos", pos.y);
         prefs.putInt("xdiv", splitPane.getDividerLocation());
         prefs.put("directory",fc.getCurrentDirectory().toString());
      } catch (Exception e) {}
      System.exit(0); 
   }

   /**About dialog.*/
   public void about() {
      JOptionPane.showMessageDialog(frame,
            "BIOCHAM (C) 2003-2007 INRIA, France,\n" +
            "by N. Chabrier-Rivier, F. Fages and S. Soliman.\n" +
            "http://contraintes.inria.fr/BIOCHAM/\n\n" +
            "Graphical User Interface\n" +
            "by L. Calzone and S. Soliman",
            "About",
            JOptionPane.INFORMATION_MESSAGE);
   }

   /** Returns an Image, or null if the path was invalid. */
   protected static Image createImage(String path) {
      java.net.URL imgURL = BiochamGUI.class.getResource(path);
      if (imgURL != null) {
         return  Toolkit.getDefaultToolkit().getImage(imgURL);
      } else {
         System.err.println("Couldn't find file: " + path);
         return null;
      }
   }

   /** Returns an ImageIcon, or null if the path was invalid. */
   protected static ImageIcon createImageIcon(String path) {
      java.net.URL imgURL = BiochamGUI.class.getResource(path);
      if (imgURL != null) {
         return new ImageIcon(imgURL);
      } else {
         System.err.println("Couldn't find file: " + path);
         return null;
      }
   }

   /**Main.*/
   public static void main(String[] args) {
      try {
         //Try tu use the Local look and feel
         UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
      } catch (Exception e1) {
         try  {
            //Else use Metal
            UIManager.setLookAndFeel("javax.swing.plaf.metal.MetalLookAndFeel");
         } catch (Exception e2) {}
      }
      arguments = args;
      app_path = new String();
      for (int i=0;i<arguments.length;++i)
         if (!arguments[i].endsWith(".bc")) {
            if (app_path.length() > 0)
               System.err.println("several PATHES on command line ?");
            else
               app_path = app_path+arguments[i];
         }

      if (app_path.equals(""))
         app_path = ".";

      createAndShowGUI();
   }
}
