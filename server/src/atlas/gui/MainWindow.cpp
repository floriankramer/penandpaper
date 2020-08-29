#include "MainWindow.h"

#include <QAction>
#include <QDialog>
#include <QDockWidget>
#include <QFileDialog>
#include <QGridLayout>
#include <QIntValidator>
#include <QLabel>
#include <QLineEdit>
#include <QMenu>
#include <QMenuBar>
#include <QString>

#include "Conversions.h"
#include "core/Map.h"
#include "rendering/TileRenderer.h"

namespace atlas {
MainWindow::MainWindow() {
  buildMenu();
  buildUi();
}

void MainWindow::buildUi() {
  _map = std::make_shared<Map>();
  _map->initialize(1024, 1024, 1);

  _map_widget = new MapWidget();
  _map_widget->setMap(_map);

  setCentralWidget(_map_widget);

  _brush_panel = new BrushPanel();
  _brush_panel->setMapWidget(_map_widget);

  QDockWidget *brush_dock = new QDockWidget("Brushes", this);
  brush_dock->setWidget(_brush_panel);
  addDockWidget(Qt::DockWidgetArea::LeftDockWidgetArea, brush_dock);

  _object_panel = new ObjectPanel();
  _object_panel->setMapWidget(_map_widget);
  _object_panel->setMap(_map);

  QDockWidget *object_dock = new QDockWidget("Objects", this);
  object_dock->setWidget(_object_panel);
  addDockWidget(Qt::DockWidgetArea::RightDockWidgetArea, object_dock);
}

void MainWindow::buildMenu() {
  QMenu *file = menuBar()->addMenu("file");

  QAction *newMap = new QAction("New Map");
  newMap->setShortcut(QKeySequence::New);
  connect(newMap, &QAction::triggered, [this]() { this->newMap(); });

  file->addAction(newMap);

  QAction *saveMap = new QAction("Save");
  saveMap->setShortcut(QKeySequence::Save);
  connect(saveMap, &QAction::triggered, [this]() {
    QString path =
        QFileDialog::getSaveFileName(this, "Save", "", "map (*.map)");
    if (path.size() > 0) {
      _map->save(path.toUtf8().constData());
    }
  });
  file->addAction(saveMap);

  QAction *loadMap = new QAction("Load");
  loadMap->setShortcut(QKeySequence::Open);
  connect(loadMap, &QAction::triggered, [this]() {
    QString path =
        QFileDialog::getOpenFileName(this, "Load", "", "map (*.map)");
    if (path.size() > 0) {
      _map->load(path.toUtf8().constData());
      _map_widget->setMap(_map);
      _object_panel->setMap(_map);
    }
  });
  file->addAction(loadMap);

  QAction *exportMap = new QAction("Export");
  connect(exportMap, &QAction::triggered, [this]() {
    QString path =
        QFileDialog::getSaveFileName(this, "Export", "", "png (*.png)");
    if (path.size() > 0) {
      _map_widget->exportMap(path.toUtf8().constData());
    }
  });
  file->addAction(exportMap);

  QMenu *edit = menuBar()->addMenu("Edit");

  QAction *undo = new QAction("undo");
  undo->setShortcut(QKeySequence::Undo);
  connect(undo, &QAction::triggered, [this]() { _map_widget->undo(); });

  edit->addAction(undo);
}

void MainWindow::newMap() {
  QDialog *dialog = new QDialog();

  QGridLayout *layout = new QGridLayout();
  int row = 0;

  layout->addWidget(new QLabel("Width"), row, 0);
  QLineEdit *width_edit = new QLineEdit("1024");
  width_edit->setValidator(new QIntValidator(0, 9999999));
  layout->addWidget(width_edit, row, 1);
  row++;

  layout->addWidget(new QLabel("Height"), row, 0);
  QLineEdit *height_edit = new QLineEdit("1024");
  height_edit->setValidator(new QIntValidator(0, 9999999));
  layout->addWidget(height_edit, row, 1);
  row++;

  QPushButton *cancel = new QPushButton("Cancel");
  layout->addWidget(cancel, row, 0);
  connect(cancel, &QPushButton::pressed, dialog, &QDialog::reject);

  QPushButton *accept = new QPushButton("Create");
  layout->addWidget(accept, row, 1);
  connect(accept, &QPushButton::pressed, dialog, &QDialog::accept);

  dialog->setLayout(layout);
  int r = dialog->exec();
  if (r == QDialog::Accepted) {
    int width = width_edit->text().toInt();
    int height = height_edit->text().toInt();
    _map->initialize(width, height, 1);
    _map_widget->setMap(_map);
    _object_panel->setMap(_map);
  }
}
}  // namespace atlas
