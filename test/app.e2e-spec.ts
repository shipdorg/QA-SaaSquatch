import { HttpStatus, INestApplication } from '@nestjs/common';
import { Test, TestingModule } from '@nestjs/testing';
import * as request from 'supertest';
import { getSystemVersion } from '../fixtures/r';
import { AppModule } from './../src/app.module';

const { OK } = HttpStatus;

describe('AppController (e2e)', () => {
  const rversion = getSystemVersion();
  let app: INestApplication;

  beforeEach(async () => {
    const moduleFixture: TestingModule = await Test.createTestingModule({
      imports: [AppModule],
    }).compile();

    app = moduleFixture.createNestApplication();
    await app.init();
  });

  it('/ (GET)', () => {
    return request(app.getHttpServer())
      .get('/')
      .expect(200)
      .expect('Hello World!');
  });

  describe('GET /rversion', () => {
    it(`It should respond with a HTTP ${OK} and the current R version`, () => {
      return request(app.getHttpServer())
        .get('/rversion')
        .expect(OK)
        .expect({ rversion });
    });
  });
});
