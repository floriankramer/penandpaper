#include <QApplication>
#include "MainWindow.h"

int main(int argc, char **argv) {
  QApplication app(argc, argv);
  atlas::MainWindow window;
  window.show();
  app.exec();
}
