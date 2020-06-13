#include "MainWindow.h"

#include <QAction>
#include <QLabel>
#include <QMenu>
#include <QMenuBar>

#include "Conversions.h"
#include "core/Map.h"
#include "rendering/TileRenderer.h"

namespace atlas {
MainWindow::MainWindow() {
  buildMenu();
  buildUi();
}

void MainWindow::buildUi() {
  Map map;
  // initialize a 500x500m map
  map.initialize(10, 10, 50);
  TileRenderer renderer;
  Image img = renderer.renderTile(&map, 0, 0, 0, 512, 512);
  QImage qimg = Conversions::qImageFromImage(img);

  QLabel *label = new QLabel("Hello There");
  label->setPixmap(QPixmap::fromImage(qimg));
  setCentralWidget(label);
}

void MainWindow::buildMenu() {
  QMenu *file = menuBar()->addMenu("file");

  QAction *newMap = new QAction("New Map");

  file->addAction(newMap);
}
}  // namespace atlas
