#include "BrushPanel.h"

#include <QFrame>
#include <QLabel>
#include <QSlider>
#include <QSpacerItem>
#include <QVBoxLayout>

#include "MapWidget.h"

namespace atlas {

BrushPanel::BrushPanel(QWidget *parent) : QWidget(parent) { createUi(); }

BrushPanel::~BrushPanel() {}

void BrushPanel::setMapWidget(MapWidget *map_widget) {
  _map_widget = map_widget;
  connect(_add_button, &QPushButton::pressed,
          [this] { _map_widget->setBrush(MapWidget::Brush::ADD); });
  connect(_subtract_button, &QPushButton::pressed,
          [this] { _map_widget->setBrush(MapWidget::Brush::SUBTRACT); });
  connect(_noise_button, &QPushButton::pressed,
          [this] { _map_widget->setBrush(MapWidget::Brush::NOISE); });
  connect(_smoothe_button, &QPushButton::pressed,
          [this] { _map_widget->setBrush(MapWidget::Brush::SMOOTHE); });
  connect(_sharpen_button, &QPushButton::pressed,
          [this] { _map_widget->setBrush(MapWidget::Brush::SHARPEN); });

  _brush_size_slider->setValue(map_widget->brushSize());
  connect(_brush_size_slider, &QSlider::valueChanged,
          [this](int val) { _map_widget->setBrushSize(val); });

  _brush_strength_slider->setValue(map_widget->brushStrength() * 100);
  connect(_brush_strength_slider, &QSlider::valueChanged,
          [this](int val) { _map_widget->setBrushStrength(val / 100.0f); });
}

void BrushPanel::createUi() {
  QVBoxLayout *layout = new QVBoxLayout();

  _add_button = new QPushButton("add");
  layout->addWidget(_add_button);

  _subtract_button = new QPushButton("subtract");
  layout->addWidget(_subtract_button);

  _noise_button = new QPushButton("noise");
  layout->addWidget(_noise_button);

  _smoothe_button = new QPushButton("smoothe");
  layout->addWidget(_smoothe_button);

  _sharpen_button = new QPushButton("sharpen");
  layout->addWidget(_sharpen_button);

  QFrame *sep = new QFrame(this);
  sep->setFrameShape(QFrame::HLine);
  sep->setFrameStyle(QFrame::Sunken);
  layout->addWidget(sep);

  layout->addWidget(new QLabel("Brush Size"));
  _brush_size_slider = new QSlider();
  _brush_size_slider->setOrientation(Qt::Horizontal);
  _brush_size_slider->setRange(1, 100);
  _brush_size_slider->setValue(12);
  QLabel *brush_size_label = new QLabel("12");

  connect(_brush_size_slider, &QSlider::valueChanged,
          [brush_size_label](int val) {
            brush_size_label->setText(QString::number(val));
          });

  layout->addWidget(_brush_size_slider);
  layout->addWidget(brush_size_label);

  layout->addWidget(new QLabel("Brush Strength"));
  _brush_strength_slider = new QSlider();
  _brush_strength_slider->setOrientation(Qt::Horizontal);
  _brush_strength_slider->setRange(0, 100);
  _brush_strength_slider->setValue(100);
  QLabel *brush_strength_label = new QLabel("100%");

  connect(_brush_strength_slider, &QSlider::valueChanged,
          [brush_strength_label](int val) {
            brush_strength_label->setText(QString::number(val) + "%");
          });

  layout->addWidget(_brush_strength_slider);
  layout->addWidget(brush_strength_label);

  layout->addWidget(new QWidget(), 1);

  setLayout(layout);
}

}  // namespace atlas
