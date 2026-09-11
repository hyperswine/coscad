# Homebrew formula for CoScad.
#
#   brew tap hyperswine/coscad https://github.com/hyperswine/coscad
#   brew install coscad            # tagged release (source build with Stack)
#   brew install --HEAD coscad     # current main
#
# scripts/update-formula.sh <version> refreshes `url`/`sha256` after a tag.
class Coscad < Formula
  desc 'Glyph/pipeline CAD language: .coscad/.assemble to OpenSCAD, STL, and print beds'
  homepage 'https://github.com/hyperswine/coscad'
  url 'https://github.com/hyperswine/coscad/archive/refs/tags/v1.1.0.0.tar.gz'
  sha256 '0000000000000000000000000000000000000000000000000000000000000000' # scripts/update-formula.sh
  license 'GPL-3.0-only'
  head 'https://github.com/hyperswine/coscad.git', branch: 'main'

  depends_on 'ghc@9.8' => :build
  depends_on 'haskell-stack' => :build

  def install
    # stack.yaml uses the system GHC (snapshot: 9.8.x); Homebrew's ghc@9.8 is keg-only.
    ENV.prepend_path 'PATH', formula_opt_bin('ghc@9.8')
    system 'stack', '--system-ghc', '--no-install-ghc', '--local-bin-path', bin, 'install'
    man1.install 'man/coscad.1'
    doc.install Dir['docs/*.md'], 'README.md', 'CHANGELOG.md'
    pkgshare.install 'examples'
  end

  def caveats
    <<~EOS
      Rendering (coscad stl / next / check) needs OpenSCAD and the BOSL2 library:
        brew install --cask openscad
        git clone https://github.com/BelfrySCAD/BOSL2 ~/Documents/OpenSCAD/libraries/BOSL2
      Verify the chain with: coscad doctor
      Examples: #{opt_pkgshare}/examples    Manual: man coscad
    EOS
  end

  test do
    assert_match version.to_s, shell_output("#{bin}/coscad --version")
    (testpath / 't.coscad').write "r = 5\nmain = ● r |> cutat top 0 0 0 (zcyl 1 20)\n"
    system bin / 'coscad', testpath / 't.coscad'
    scad = (testpath / 't.scad').read
    assert_match 'sphere(5)', scad
    assert_match 'zcyl(r = 1, l = 20)', scad
    assert_match 'difference()', scad
  end
end
