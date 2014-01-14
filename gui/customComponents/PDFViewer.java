package fr.inria.contraintes.biocham.customComponents;


/*
 * $Id: PDFViewer.java,v 1.8 2009/01/26 05:07:18 tomoke Exp $
 *
 * Copyright 2004 Sun Microsystems, Inc., 4150 Network Circle,
 * Santa Clara, California 95054, U.S.A. All rights reserved.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 */


import com.sun.pdfview.Flag;
import com.sun.pdfview.FullScreenWindow;
import com.sun.pdfview.OutlineNode;
import com.sun.pdfview.PDFDestination;
import com.sun.pdfview.PDFFile;
import com.sun.pdfview.PDFObject;
import com.sun.pdfview.PDFPage;
import com.sun.pdfview.PageChangeListener;
import com.sun.pdfview.PagePanel;
import com.sun.pdfview.ThumbPanel;
import com.sun.pdfview.action.GoToAction;
import com.sun.pdfview.action.PDFAction;

import fr.inria.contraintes.biocham.BiochamMainFrame;
import fr.inria.contraintes.biocham.utils.Utils;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.DataInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.RandomAccessFile;
import java.net.URL;
import java.net.URLConnection;
import java.nio.ByteBuffer;
import java.nio.channels.FileChannel;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.Box;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JFileChooser;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JSplitPane;
import javax.swing.JTextField;
import javax.swing.JToggleButton;
import javax.swing.JToolBar;
import javax.swing.SwingUtilities;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;
import javax.swing.filechooser.FileFilter;

/**
 * PDF Viewer for the Documentation sector of the Biocham Graphical Interface.
 * 
 * Modified by Dragana Jovanovska 
 */
public class PDFViewer extends JPanel implements KeyListener, TreeSelectionListener, PageChangeListener {

  
    /** The current PDFFile */
    PDFFile curFile;
    /** the name of the current document */
    String docName;
    /** The split between thumbs and page */
    JSplitPane split;
    /** The thumbnail scroll pane */
    UniversalScrollPane thumbscroll;
    /** The thumbnail display */
    ThumbPanel thumbs;
    
    /** The page display */
    PagePanel page;
    /** The full screen page display, or null if not in full screen mode */
    PagePanel fspp;

    
    
    //    Thread anim;
    /** The current page number (starts at 0), or -1 if no page */
    int curpage = -1;
    /** the full screen button */
    JToggleButton fullScreenButton;
   
    /** the current page number text field */
    JTextField pageField;    
    /** the full screen window, or null if not in full screen mode */
    FullScreenWindow fullScreen;
    /** the root of the outline, or null if there is no outline */
    OutlineNode outline = null;
    
    /** The page format for printing */
   // PageFormat pformat = PrinterJob.getPrinterJob().defaultPage();
    
    /** true if the thumb panel should exist at all */
    boolean doThumb = true;
    /** flag to indicate when a newly added document has been announced */
    Flag docWaiter;
    /** a thread that pre-loads the next page for faster response */
  //  PagePreparer pagePrep;
    /** the window containing the pdf outline, or null if one doesn't exist */
    JDialog olf;
    
    
    
    class ThumbAction extends AbstractAction
            implements PropertyChangeListener {

        boolean isOpen = true;

        public ThumbAction() {
            super("Hide thumbnails");
        }

        public void propertyChange(PropertyChangeEvent evt) {
            int v = ((Integer) evt.getNewValue()).intValue();
            if (v <= 1) {
                isOpen = false;
                putValue(ACTION_COMMAND_KEY, "Show thumbnails");
                putValue(NAME, "Show thumbnails");
            } else {
                isOpen = true;
                putValue(ACTION_COMMAND_KEY, "Hide thumbnails");
                putValue(NAME, "Hide thumbnails");
            }
        }

        public void actionPerformed(ActionEvent evt) {
            doThumbs(!isOpen);
        }
    }
    ThumbAction thumbAction = new ThumbAction();
   
    
    Action fullScreenAction = new AbstractAction("Full screen",
            Utils.createImageIcon("projectImages/")) {

        public void actionPerformed(ActionEvent evt) {
            doFullScreen((evt.getModifiers() & evt.SHIFT_MASK) != 0);
        }
    };
    Action nextAction = new AbstractAction("Next", Utils.createImageIcon("projectImages/")) {

        public void actionPerformed(ActionEvent evt) {
            doNext();
        }
    };
    Action firstAction = new AbstractAction("First",Utils.createImageIcon("projectImages/")) {

        public void actionPerformed(ActionEvent evt) {
            doFirst();
        }
    };
    Action lastAction = new AbstractAction("Last", Utils.createImageIcon("projectImages/")) {

        public void actionPerformed(ActionEvent evt) {
            doLast();
        }
    };
    Action prevAction = new AbstractAction("Prev", Utils.createImageIcon("projectImages/")) {

        public void actionPerformed(ActionEvent evt) {
            doPrev();
        }
    };

    /**
     * Create a new PDFViewer based on a user, with or without a thumbnail
     * panel.
     * @param useThumbs true if the thumb panel should exist, false if not.
     */
    public PDFViewer(boolean useThumbs) {
    	super(new BorderLayout());    	
    	super.setBackground(Color.WHITE);
    	
    	boolean b=SwingUtilities.isEventDispatchThread();
    	
    	
        doThumb = useThumbs;
        init();
    }

    /**
     * Initialize this PDFViewer by creating the GUI.
     */
    protected void init() {
    	
    	boolean b=SwingUtilities.isEventDispatchThread();
    	
    	
        page = new PagePanel();
        page.setSize(getSize());
        page.setBackground(Color.WHITE);
        page.addKeyListener(this);

        if (doThumb) {
            split = new JSplitPane(split.HORIZONTAL_SPLIT);
            split.addPropertyChangeListener(split.DIVIDER_LOCATION_PROPERTY,
                    thumbAction);
            split.setOneTouchExpandable(true);
            thumbs = new ThumbPanel(null);
            thumbs.setBackground(Color.WHITE);
            thumbscroll = new UniversalScrollPane(thumbs);
            thumbscroll.setVerticalScrollBarPolicy(UniversalScrollPane.VERTICAL_SCROLLBAR_ALWAYS);
            thumbscroll.setHorizontalScrollBarPolicy(UniversalScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
            split.setLeftComponent(thumbscroll);
            split.setRightComponent(new UniversalScrollPane(page));
            add(split, BorderLayout.CENTER);
        } else {
            add(new UniversalScrollPane(page), BorderLayout.CENTER);
        }

        JToolBar toolbar = new JToolBar();
        toolbar.setFloatable(false);
        toolbar.setBackground(Utils.backgroundColor);
        JButton jb;

        jb = new JButton(firstAction);
        jb.setText("First");
        toolbar.add(jb);
        jb = new JButton(prevAction);
        jb.setText("Previous");
        toolbar.add(jb);
        pageField = new JTextField("-", 3);
        pageField.setForeground(Color.BLUE);
        pageField.setHorizontalAlignment(JTextField.CENTER);
        pageField.setFont(new Font("",Font.BOLD,12));
        pageField.setMaximumSize(new Dimension(45, 32));
        pageField.addActionListener(new ActionListener() {

            public void actionPerformed(ActionEvent evt) {
                doPageTyped();
            }
        });
        toolbar.add(pageField);
        jb = new JButton(nextAction);
        jb.setText("Next");
        toolbar.add(jb);
        jb = new JButton(lastAction);
        jb.setText("Last");
        toolbar.add(jb);

        toolbar.add(Box.createHorizontalGlue());

        fullScreenButton = new JToggleButton(fullScreenAction);
        fullScreenButton.setText("Full Screen");
        toolbar.add(fullScreenButton);
        fullScreenButton.setEnabled(true);

        toolbar.add(Box.createHorizontalGlue());
        toolbar.add(Box.createHorizontalGlue());
        add(toolbar, BorderLayout.NORTH);

    }

    /**
     * Changes the displayed page, desyncing if we're not on the
     * same page as a presenter.
     * @param pagenum the page to display
     */
    public void gotoPage(int pagenum) {
        if (pagenum < 0) {
            pagenum = 0;
        } else if (pagenum >= curFile.getNumPages()) {
            pagenum = curFile.getNumPages() - 1;
        }
        forceGotoPage(pagenum);
    }

    /**
     * Changes the displayed page.
     * @param pagenum the page to display
     */
    public void forceGotoPage(int pagenum) {
        if (pagenum <= 0) {
            pagenum = 0;
        } else if (pagenum >= curFile.getNumPages()) {
            pagenum = curFile.getNumPages() - 1;
        }
        curpage = pagenum;

        // update the page text field
        pageField.setText(String.valueOf(curpage + 1));

        // fetch the page and show it in the appropriate place
        PDFPage pg = curFile.getPage(pagenum + 1);
        if (fspp != null) {
            fspp.showPage(pg);
            fspp.requestFocus();
        } else {
            page.showPage(pg);
            page.requestFocus();
        }

        // update the thumb panel
        if (doThumb) {
            thumbs.pageShown(pagenum);
        }

        // stop any previous page prepper, and start a new one
//        if (pagePrep != null) {
//            pagePrep.quit();
//        }
//        pagePrep = new PagePreparer(pagenum);
//        pagePrep.start();

        setEnabling();
    }

   

    /**
     * Enable or disable all of the actions based on the current state.
     */
    public void setEnabling() {
        boolean fileavailable = curFile != null;
        boolean pageshown = ((fspp != null) ? fspp.getPage() != null : page.getPage() != null);
        pageField.setEnabled(fileavailable);
        fullScreenAction.setEnabled(pageshown);
        prevAction.setEnabled(pageshown);
        nextAction.setEnabled(pageshown);
        firstAction.setEnabled(fileavailable);
        lastAction.setEnabled(fileavailable);
    }

    /**
     * open a URL to a PDF file. The file is read in and processed
     * with an in-memory buffer.
     *
     * @param url
     * @throws java.io.IOException
     */
    public void openFile(URL url) throws IOException {
        URLConnection urlConnection = url.openConnection();
        int contentLength = urlConnection.getContentLength();
        InputStream istr = urlConnection.getInputStream();
        byte[] byteBuf = new byte[contentLength];
        int offset = 0;
        int read = 0;
        while (read >= 0) {
            read = istr.read(byteBuf, offset, contentLength - offset);
            if (read > 0) {
                offset += read;
            }
        }
        if (offset != contentLength) {
            throw new IOException("Could not read all of URL file.");
        }
        ByteBuffer buf = ByteBuffer.allocate(contentLength);
        buf.put(byteBuf);
        openPDFByteBuffer(buf, url.toString(), url.getFile());
    }

    /**
     * <p>Open a specific pdf file.  Creates a DocumentInfo from the file,
     * and opens that.</p>
     *
     * <p><b>Note:</b> Mapping the file locks the file until the PDFFile
     * is closed.</p>
     *
     * @param file the file to open
     * @throws IOException
     */
    public void openFile(File file) throws IOException {
    
        // first open the file for random access
        RandomAccessFile raf = new RandomAccessFile(file, "r");

        // extract a file channel
        FileChannel channel = raf.getChannel();

        // now memory-map a byte-buffer
        ByteBuffer buf =
                channel.map(FileChannel.MapMode.READ_ONLY, 0, channel.size());
        openPDFByteBuffer(buf, file.getPath(), file.getName());
    }

    /**
     * <p>Open a specific pdf file.  Creates a DocumentInfo from the file,
     * and opens that.</p>
     *
     * <p><b>Note:</b> By not memory mapping the file its contents are
     * not locked down while PDFFile is open.</p>
     *
     * @param file the file to open
     */
    public void openFileUnMapped(File file) throws IOException {
        DataInputStream istr = null;
        try {
            //load a pdf from a byte buffer
            // avoid using a RandomAccessFile but fill a ByteBuffer directly
            istr = new DataInputStream(new FileInputStream(file));
            long len = file.length();
            if (len > Integer.MAX_VALUE) {
                throw new IOException("File too long to decode: " + file.getName());
            }
            int contentLength = (int) len;
            byte[] byteBuf = new byte[contentLength];
            int offset = 0;
            int read = 0;
            while (read >= 0) {
                read = istr.read(byteBuf, offset, contentLength - offset);
                if (read > 0) {
                    offset += read;
                }
            }
            ByteBuffer buf = ByteBuffer.allocate(contentLength);
            buf.put(byteBuf);
            openPDFByteBuffer(buf, file.getPath(), file.getName());
        } catch (FileNotFoundException fnfe) {
            fnfe.printStackTrace();
        } catch (IOException ioe) {
            ioe.printStackTrace();
        } finally {
            if (istr != null) {
                try {
                    istr.close();
                } catch (Exception e) {
                	e.printStackTrace();
                    // ignore error on close
                }
            }
        }
    }

    /**
     * open the ByteBuffer data as a PDFFile and start to process it.
     *
     * @param buf
     * @param path
     */
    private void openPDFByteBuffer(ByteBuffer buf, String path, String name) {

        // create a PDFFile from the data
        PDFFile newfile = null;
        try {
            newfile = new PDFFile(buf);
        } catch (IOException ioe) {
            openError(path + " doesn't appear to be a PDF file.");
            return;
        }

        // Now that we're reasonably sure this document is real, close the
        // old one.
       // doClose();

        // set up our document
        this.curFile = newfile;
        docName = name;
        setName(docName);

        // set up the thumbnails
        if (doThumb) {
            thumbs = new ThumbPanel(curFile);
            thumbs.addPageChangeListener(this);
            thumbscroll.getViewport().setView(thumbs);
            thumbscroll.getViewport().setBackground(Color.gray);
        }

        setEnabling();

        // display page 1.
        forceGotoPage(0);

        
    }

    /**
     * Display a dialog indicating an error.
     */
    public void openError(String message) {
        JOptionPane.showMessageDialog(split, message, "Error opening file",
                JOptionPane.ERROR_MESSAGE);
    }
    /**
     * A file filter for PDF files.
     */
    /*FileFilter pdfFilter = new FileFilter() {

        public boolean accept(File f) {
            return f.isDirectory() || f.getName().endsWith(".pdf");
        }

        public String getDescription() {
            return "Choose a PDF file";
        }
    };*/
    private File prevDirChoice;

    /**
     * Ask the user for a PDF file to open from the local file system
     */
    public void doOpen() {
        try {
        	Utils.fileChooser.setCurrentDirectory(prevDirChoice);
        	//Utils.fileChooser.setFileFilter(Utils.getFileFilter("pdf"));//pdfFilter
        	Utils.fileChooser.setMultiSelectionEnabled(false);
        	Utils.fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
        	String rep=Utils.showOpenDialog(BiochamMainFrame.frame,"pdf");			
        	if (rep!=null) {
                  
                try {
                    prevDirChoice = new File(rep);
                    openFile(prevDirChoice);
                } catch (IOException ioe) {
                    ioe.printStackTrace();
                }
            }
        } catch (Exception e) {
            JOptionPane.showMessageDialog(split,
                    "Opening files from your local " +
                    "disk is not available\nfrom the " +
                    "Java Web Start version of this " +
                    "program.\n",
                    "Error opening directory",
                    JOptionPane.ERROR_MESSAGE);
            e.printStackTrace();
        }
    }

    /**
     * Open a local file, given a string filename
     * @param name the name of the file to open
     */
    public void doOpen(String name) {
        try {
            URL url = new URL(name);
            openFile(new URL(name));
        } catch (IOException ioe) {
            try {
                openFile(new File(name));
            } catch (IOException ex) {
                Logger.getLogger(PDFViewer.class.getName()).log(Level.SEVERE, null, ex);
            }
        }
    }

   
    /**
     * Shows or hides the thumbnails by moving the split pane divider
     */
    public void doThumbs(boolean show) {
        if (show) {
            split.setDividerLocation((int) thumbs.getPreferredSize().width +
                    (int) thumbscroll.getVerticalScrollBar().
                    getWidth() + 4);
        } else {
            split.setDividerLocation(0);
        }
    }

    /**
     * Enter full screen mode
     * @param force true if the user should be prompted for a screen to
     * use in a multiple-monitor setup.  If false, the user will only be
     * prompted once.
     */
    public void doFullScreen(boolean force) {
        setFullScreenMode(fullScreen == null, force);
    }

  
    /**
     * Goes to the next page
     */
    public void doNext() {
        gotoPage(curpage + 1);
    }

    /**
     * Goes to the previous page
     */
    public void doPrev() {
        gotoPage(curpage - 1);
    }

    /**
     * Goes to the first page
     */
    public void doFirst() {
        gotoPage(0);
    }

    /**
     * Goes to the last page
     */
    public void doLast() {
        gotoPage(curFile.getNumPages() - 1);
    }

    /**
     * Goes to the page that was typed in the page number text field
     */
    public void doPageTyped() {
        int pagenum = -1;
        try {
            pagenum = Integer.parseInt(pageField.getText()) - 1;
        } catch (NumberFormatException nfe) {
        }
        if (pagenum >= curFile.getNumPages()) {
            pagenum = curFile.getNumPages() - 1;
        }
        if (pagenum >= 0) {
            if (pagenum != curpage) {
                gotoPage(pagenum);
            }
        } else {
            pageField.setText(String.valueOf(curpage));
        }
    }

    /**
     * Runs the FullScreenMode change in another thread
     */
    class PerformFullScreenMode implements Runnable {

        boolean force;

        public PerformFullScreenMode(boolean forcechoice) {
            force = forcechoice;
        }

        public void run() {
            fspp = new PagePanel();
            fspp.setBackground(Color.black);
            page.showPage(null);
            fullScreen = new FullScreenWindow(fspp, force);
            fspp.addKeyListener(PDFViewer.this);
            gotoPage(curpage);
            fullScreenAction.setEnabled(true);
        }
    }

    /**
     * Starts or ends full screen mode.
     * @param full true to enter full screen mode, false to leave
     * @param force true if the user should be prompted for a screen
     * to use the second time full screen mode is entered.
     */
    public void setFullScreenMode(boolean full, boolean force) {
        //	curpage= -1;
        if (full && fullScreen == null) {
            fullScreenAction.setEnabled(false);
            new Thread(new PerformFullScreenMode(force),
                    getClass().getName() + ".setFullScreenMode").start();
            fullScreenButton.setSelected(true);
        } else if (!full && fullScreen != null) {
            fullScreen.close();
            fspp = null;
            fullScreen = null;
            gotoPage(curpage);
            fullScreenButton.setSelected(false);
        }
    }


    /**
     * Handle a key press for navigation
     */
    public void keyPressed(KeyEvent evt) {
        int code = evt.getKeyCode();
        if (code == evt.VK_LEFT) {
            doPrev();
        } else if (code == evt.VK_RIGHT) {
            doNext();
        } else if (code == evt.VK_UP) {
            doPrev();
        } else if (code == evt.VK_DOWN) {
            doNext();
        } else if (code == evt.VK_HOME) {
            doFirst();
        } else if (code == evt.VK_END) {
            doLast();
        } else if (code == evt.VK_PAGE_UP) {
            doPrev();
        } else if (code == evt.VK_PAGE_DOWN) {
            doNext();
        } else if (code == evt.VK_SPACE) {
            doNext();
        } else if (code == evt.VK_ESCAPE) {
            setFullScreenMode(false, false);
        }
    }

    /**
     * Combines numeric key presses to build a multi-digit page number.
     */
    class PageBuilder implements Runnable {

        int value = 0;
        long timeout;
        Thread anim;
        static final long TIMEOUT = 500;

        /** add the digit to the page number and start the timeout thread */
        public synchronized void keyTyped(int keyval) {
            value = value * 10 + keyval;
            timeout = System.currentTimeMillis() + TIMEOUT;
            if (anim == null) {
                anim = new Thread(this);
                anim.setName(getClass().getName());
                anim.start();
            }
        }

        /**
         * waits for the timeout, and if time expires, go to the specified
         * page number
         */
        public void run() {
            long now, then;
            synchronized (this) {
                now = System.currentTimeMillis();
                then = timeout;
            }
            while (now < then) {
                try {
                    Thread.sleep(timeout - now);
                } catch (InterruptedException ie) {
                }
                synchronized (this) {
                    now = System.currentTimeMillis();
                    then = timeout;
                }
            }
            synchronized (this) {
                gotoPage(value - 1);
                anim = null;
                value = 0;
            }
        }
    }
    PageBuilder pb = new PageBuilder();

    public void keyReleased(KeyEvent evt) {
    }

    /**
     * gets key presses and tries to build a page if they're numeric
     */
    public void keyTyped(KeyEvent evt) {
        char key = evt.getKeyChar();
        if (key >= '0' && key <= '9') {
            int val = key - '0';
            pb.keyTyped(val);
        }
    }

    /**
     * Someone changed the selection of the outline tree.  Go to the new
     * page.
     */
    public void valueChanged(TreeSelectionEvent e) {
        if (e.isAddedPath()) {
            OutlineNode node = (OutlineNode) e.getPath().getLastPathComponent();
            if (node == null) {
                return;
            }

            try {
                PDFAction action = node.getAction();
                if (action == null) {
                    return;
                }

                if (action instanceof GoToAction) {
                    PDFDestination dest = ((GoToAction) action).getDestination();
                    if (dest == null) {
                        return;
                    }

                    PDFObject page = dest.getPage();
                    if (page == null) {
                        return;
                    }

                    int pageNum = curFile.getPageNumber(page);
                    if (pageNum >= 0) {
                        gotoPage(pageNum);
                    }
                }
            } catch (IOException ioe) {
                ioe.printStackTrace();
            }
        }
    }
    
    
}
